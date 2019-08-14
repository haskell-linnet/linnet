{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Linnet.Endpoint
  ( EndpointResult(..)
  , Endpoint(..)
  , isMatched
  , maybeReminder
  , lift
  , liftOutputM
  , mapM'
  , mapOutput
  , mapOutputM
  , handle
  , handleAll
  , try
  , transformOutput
  , transform
  , (~>)
  , (~>>)
  , productWith
  , (//)
  , (|+|)
  , root
  , zero
  ) where

import           Control.Applicative       (Alternative (..))
import           Control.Exception         (Exception, SomeException)
import qualified Control.Monad.Catch       as MC
import           GHC.Base                  (liftA2)
import           Linnet.Errors             (LinnetError (..))
import           Linnet.Input
import           Linnet.Internal.Coproduct
import           Linnet.Internal.HList
import           Linnet.Output
import           Network.Wai               (Request)

infixl 0 ~>

infixl 0 ~>>

infixr 2 //

infixl 2 |+|

-- | Result of returned by 'Endpoint' that could be either:
--
--    * @Matched@ containing reminder of the input together with 'Output' inside of monad @m@
--
--    * @NotMatched@ in case endpoint doesn't match the input
data EndpointResult (m :: * -> *) a
  = Matched
      { matchedReminder :: Input
      , matchedOutput   :: m (Output a)
      }
  | NotMatched

isMatched :: EndpointResult m a -> Bool
isMatched (Matched _ _) = True
isMatched _             = False

maybeReminder :: EndpointResult m a -> Maybe Input
maybeReminder (Matched r _) = Just r
maybeReminder _             = Nothing

instance (Show (m (Output a))) => Show (EndpointResult m a) where
  show (Matched _ out) = "EndpointResult.Matched(" ++ show out ++ ")"
  show NotMatched      = "EndpointResult.NotMatched"

instance (Functor m) => Functor (EndpointResult m) where
  fmap f (Matched r m) = Matched r $ (fmap . fmap) f m
  fmap _ NotMatched    = NotMatched

-- | Basic Linnet data type that abstracts away operations over HTTP communication.
-- While WAI Application has type of @Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived@,
-- it's practical to treat web applications as functions of @Request -> BusinessLogic -> IO Response@ where @BusinessLogic@
-- is usually a function of @a -> m b@ where @a@ and @b@ are data to be decoded from the request / encoded to response, @m@
-- is some monad, and this is the most interesting part of an application.
--
-- Endpoint's purpose is exactly to abstract details of encoding and decoding, along with routing and the rest, and provide
-- simple interface to encapsulate @BusinessLogic@ into a final web application.
--
-- Business logic is encoded as transformation in @fmap@, @mapOutput@, @mapOutputM@, @mapM@ and the like.
-- Usual way to transform endpoint is to use '~>' and '~>>' operators:
--
-- > get (path @Text) ~> (\segment -> return $ ok segment)
-- Here, '~>' is just an inverted alias for `mapOutputM` function. Often, endpoint is a product of multiple endpoints,
-- and here '~>>' proves to be very handy:
--
-- > get (p' "sum" // path @Int // path @Int) ~>> (\i1 i2 -> return $ ok (i1 + i2) )
--
-- The trick is that '//' defines sequential @AND@ combination of endpoints that is represented as endpoint of 'HList', so
-- instead of dealing with heterogeneous list, it's possible to use '~>>' instead and map with a function of multiple arguments.
--
-- Endpoints are also composable in terms of @OR@ logic with '|+|' operator that is useful for routing:
--
-- > getUsers = get (p' "users") ~>> (ok <$> fetchUsers)
-- > newUser = post (p' "users" // jsonBody @User) ~>> (\user -> ok <$> createUser user)
-- > usersApi = getUsers |+| newUser
--
-- An endpoint might be converted into WAI @Application@ using 'Linnet.Bootstrap.bootstrap' and @\@TypeApplications@ language pragma:
--
-- > main = run 9000 app
-- >        where app = bootstrap @TextPlain usersApi & compile & toApp id
data Endpoint (m :: * -> *) a =
  Endpoint
    { runEndpoint :: Input -> EndpointResult m a
    , toString    :: String
    }

instance Show (Endpoint m a) where
  show e = "Endpoint(" ++ toString e ++ ")"

instance (Functor m) => Functor (Endpoint m) where
  fmap f e = Endpoint {runEndpoint = fmap f . runEndpoint e, toString = toString e}

instance (MC.MonadCatch m) => Applicative (Endpoint m) where
  pure a = Endpoint {runEndpoint = \i -> Matched i $ pure (ok a), toString = "const"}
  liftA2 fn fa fb = productWith fa fb fn

instance (MC.MonadCatch m) => Alternative (Endpoint m) where
  empty = Endpoint {runEndpoint = const NotMatched, toString = "empty"}
  (<|>) ea eb =
    Endpoint
      { runEndpoint =
          \input ->
            case runEndpoint ea input of
              a@(Matched remA _) ->
                case runEndpoint eb input of
                  b@(Matched remB _) ->
                    if length (reminder remA) <= length (reminder remB)
                      then a
                      else b
                  NotMatched -> a
              NotMatched -> runEndpoint eb input
      , toString = toString ea ++ "<|>" ++ toString eb
      }

-- | Map over the 'Output' of endpoint with function returning new value @a@ lifted in monad @m@
mapM' :: (Monad m) => (a -> m b) -> Endpoint m a -> Endpoint m b
mapM' fn = transformOutput (>>= traverse fn)

-- | Map over the value of 'Endpoint' with function returning new @Output b@
mapOutput :: (Monad m) => (a -> Output b) -> Endpoint m a -> Endpoint m b
mapOutput fn = mapOutputM $ pure . fn

-- | Map over the value of 'Endpoint' with function returning new @m (Output b)@
mapOutputM :: (Monad m) => (a -> m (Output b)) -> Endpoint m a -> Endpoint m b
mapOutputM fn ea =
  ea
    { runEndpoint =
        \input ->
          case runEndpoint ea input of
            Matched remA ma -> Matched remA $ ma >>= transformM fn
            NotMatched      -> NotMatched
    }

transformOutput :: (m (Output a) -> m (Output b)) -> Endpoint m a -> Endpoint m b
transformOutput fn ea =
  ea
    { runEndpoint =
        \input ->
          case runEndpoint ea input of
            Matched remA ma -> Matched remA $ fn ma
            NotMatched      -> NotMatched
    }

transform :: (Monad m) => (m a -> m b) -> Endpoint m a -> Endpoint m b
transform fn ea =
  ea
    { runEndpoint =
        \input ->
          case runEndpoint ea input of
            Matched remA ma -> Matched {matchedReminder = remA, matchedOutput = ma >>= traverse (fn . pure)}
            NotMatched -> NotMatched
    }

-- | Handle exception in monad @m@ of Endpoint result using provided function that returns new 'Output'
handle :: (MC.MonadCatch m, Exception e) => (e -> m (Output a)) -> Endpoint m a -> Endpoint m a
handle fn = transformOutput $ MC.handle fn

-- | Handle all exceptions in monad @m@ of Endpoint result
handleAll :: (MC.MonadCatch m) => (SomeException -> m (Output a)) -> Endpoint m a -> Endpoint m a
handleAll fn = transformOutput $ MC.handleAll fn

-- | Lift an exception of type @e@ into 'Either'
try :: (MC.MonadCatch m, Exception e) => Endpoint m a -> Endpoint m (Either e a)
try ea =
  ea
    { runEndpoint =
        \input ->
          let traverseEither :: Either e (Output a) -> Output (Either e a)
              traverseEither (Left e)    = ok $ Left e
              traverseEither (Right out) = Right <$> out
           in case runEndpoint ea input of
                Matched remA out -> Matched {matchedReminder = remA, matchedOutput = traverseEither <$> MC.try out}
                NotMatched -> NotMatched
    }

-- | Inversed alias for 'mapOutputM'
(~>) :: (Monad m) => Endpoint m a -> (a -> m (Output b)) -> Endpoint m b
(~>) ea fn = mapOutputM fn ea

-- | Advanced version of '~>' operator that allows to map @Endpoint m (HList ls)@
-- over a function of arity N equal to N elements of HList.
-- General rule of thumb when to use this operator is whenever there is an 'HList' on the left side.
(~>>) :: (Monad m, FnToProduct fn ls (m (Output b))) => Endpoint m (HList ls) -> fn -> Endpoint m b
(~>>) ea fn = mapOutputM (fromFunction fn) ea

-- | Lift monadic value @m a@ into 'Endpoint' that always matches
lift :: (Functor m) => m a -> Endpoint m a
lift m = Endpoint {runEndpoint = \i -> Matched i $ fmap ok m, toString = "lift"}

-- | Lift monadic output @m (Output a)@ into 'Endpoint' that always matches
liftOutputM :: m (Output a) -> Endpoint m a
liftOutputM m = Endpoint {runEndpoint = \i -> Matched i m, toString = "liftOutputM"}

-- | Create product of two 'Endpoint's that sequentially match a request.
-- | If some of endpoints doesn't match a request, the final result is also non-matching
productWith ::
     forall m a b c. MC.MonadCatch m
  => Endpoint m a
  -> Endpoint m b
  -> (a -> b -> c)
  -> Endpoint m c
productWith ea eb f =
  ea
    { runEndpoint =
        \req ->
          case runEndpoint ea req of
            Matched aRem aOutM ->
              case runEndpoint eb aRem of
                Matched bRem bOutM ->
                  let out = do
                        oa <- MC.try aOutM
                        ob <- MC.try bOutM
                        product_ oa ob
                   in Matched bRem out
                NotMatched -> NotMatched
            NotMatched -> NotMatched
    }
  where
    product_ :: Either LinnetError (Output a) -> Either LinnetError (Output b) -> m (Output c)
    product_ eitherA eitherB =
      case (eitherA, eitherB) of
        (Right oa, Right ob) -> pure $ oa >>= \a -> fmap (f a) ob
        (Left a, Left b)     -> MC.throwM $ a <> b
        (Left a, _)          -> MC.throwM a
        (_, Left b)          -> MC.throwM b

-- | Create product of two 'Endpoint's that sequentially match a request and values are adjoined into 'HList'.
-- If some of endpoints doesn't match a request, the final result is also non-matching
(//) :: (MC.MonadCatch m, AdjoinHList (a ': b ': '[]) out) => Endpoint m a -> Endpoint m b -> Endpoint m (HList out)
(//) ea eb =
  Endpoint
    { runEndpoint = runEndpoint $ productWith ea eb (\a b -> adjoin (a ::: b ::: HNil))
    , toString = toString ea ++ " // " ++ toString eb
    }

-- | Create new 'Endpoint' of two endpoints, adjoining values into 'Coproduct'
-- During request resolution the following logic is applied:
--
--    * If none of endpoints match, resulting endpoint is also non-matching
--
--    * If both endpoints match, the more specific one is selected (with shorter reminder)
(|+|) ::
     forall m a b out. (MC.MonadCatch m, AdjoinCoproduct (Coproduct a (Coproduct b CNil)) out)
  => Endpoint m a
  -> Endpoint m b
  -> Endpoint m out
(|+|) ea eb = fmap left ea <|> fmap right eb
  where
    left :: a -> out
    left a = adjoinCoproduct @(Coproduct a (Coproduct b CNil)) $ Inl a
    right :: b -> out
    right b = adjoinCoproduct @(Coproduct a (Coproduct b CNil)) $ Inr (Inl b)

-- | Endpoint that always matches and returns a request from 'Input'
root :: (Applicative m) => Endpoint m Request
root =
  Endpoint
    { runEndpoint = \input -> Matched {matchedReminder = input, matchedOutput = pure . ok $ request input}
    , toString = "root"
    }

-- | Endpoint that always matches and doesn't change any reminder
zero :: (Applicative m) => Endpoint m (HList '[])
zero =
  Endpoint
    {runEndpoint = \input -> Matched {matchedReminder = input, matchedOutput = pure . ok $ HNil}, toString = "zero"}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Linnet.Endpoints.Paths
  ( path
  , pathConst
  , p'
  , pathEmpty
  , paths
  , pathAny
  ) where

import           Data.Data             (Proxy (..), Typeable, typeRep)
import           Data.Maybe            (maybeToList)
import qualified Data.Text             as T
import           Linnet.Decode
import           Linnet.Endpoint
import           Linnet.Internal.HList
import           Linnet.Output         (ok)

-- | Endpoint that tries to decode head of the current path reminder into specific type
-- If path is empty, Endpoint is not matched
-- If decoding has failed, Endpoint is not matched
path ::
     forall a m. (DecodePath a, Applicative m, Typeable a)
  => Endpoint m a
path =
  Endpoint
    { runEndpoint =
        \input ->
          case reminder input of
            [] -> NotMatched
            (h:t) ->
              case decodePath h of
                Just v -> Matched {matchedReminder = input {reminder = t}, matchedOutput = pure $ ok v}
                Nothing -> NotMatched
    , toString = show (typeRep (Proxy :: Proxy a))
    }

-- | Endpoint that matches only if the head of current path reminder is equal to some given constant value.
-- If value matches the provided constant, saves the tail of the path as a reminder
-- Otherwise resulting endpoint is not matched
pathConst :: (Applicative m) => T.Text -> Endpoint m (HList '[])
pathConst value =
  Endpoint
    { runEndpoint =
        \input ->
          case reminder input of
            [] -> NotMatched
            (h:t) ->
              if h == value
                then Matched {matchedReminder = input {reminder = t}, matchedOutput = pure $ ok HNil}
                else NotMatched
    , toString = T.unpack value
    }

-- | Short alias for pathConst
p' :: (Applicative m) => T.Text -> Endpoint m (HList '[])
p' = pathConst

-- | Endpoint that matches only against empty path reminder
pathEmpty :: Applicative m => Endpoint m (HList '[])
pathEmpty =
  Endpoint
    { runEndpoint =
        \input ->
          case reminder input of
            [] -> Matched input (pure . ok $ HNil)
            _  -> NotMatched
    , toString = "/"
    }

-- | Endpoint that consumes the rest of the path reminder and decode it using provided DecodePath for some type @a@
paths ::
     forall a m. (DecodePath a, Applicative m, Typeable a)
  => Endpoint m [a]
paths =
  Endpoint
    { runEndpoint =
        \input ->
          Matched
            { matchedReminder = input {reminder = []}
            , matchedOutput = pure $ ok (map (decodePath @a) (reminder input) >>= maybeToList)
            }
    , toString = "[" ++ show (typeRep (Proxy :: Proxy a)) ++ "]"
    }

-- | Endpoint that matches any path and discards reminder
pathAny :: (Applicative m) => Endpoint m (HList '[])
pathAny =
  Endpoint
    { runEndpoint = \input -> Matched {matchedReminder = input {reminder = []}, matchedOutput = pure . ok $ HNil}
    , toString = "*"
    }

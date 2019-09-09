{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
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
import           Linnet.Input
import           Linnet.Internal.HList
import           Linnet.Output         (ok)
import           Network.Wai           (pathInfo)

-- | Endpoint that tries to decode head of the current path reminder into specific type. It consumes head of the reminder.
--
-- * If path is empty, Endpoint is not matched
--
-- * If decoding has failed, Endpoint is not matched
path ::
     forall a m. (DecodePath a, Applicative m, Typeable a)
  => Endpoint m a
path =
  Endpoint
    { runEndpoint =
        \input ->
          case reminder input of
            [] -> NotMatched Other
            (h:t) ->
              case decodePath h of
                Just v ->
                  Matched {matchedReminder = input {reminder = t}, matchedTrace = [str], matchedOutput = pure $ ok v}
                Nothing -> NotMatched Other
    , toString = str
    }
  where
    str = T.pack $ show (typeRep (Proxy :: Proxy a))

-- | Endpoint that matches only if the head of current path reminder is equal to some given constant value.
-- It consumes head of the reminder.
--
-- * If value matches the provided constant, saves the tail of the path as a reminder
--
-- * Otherwise, resulting endpoint is not matched
pathConst :: (Applicative m) => T.Text -> Endpoint m (HList '[])
pathConst value =
  Endpoint
    { runEndpoint =
        \input ->
          case reminder input of
            [] -> NotMatched Other
            (h:t) ->
              if h == value
                then Matched
                       {matchedReminder = input {reminder = t}, matchedTrace = [h], matchedOutput = pure $ ok HNil}
                else NotMatched Other
    , toString = value
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
            [] -> Matched {matchedReminder = input, matchedTrace = [], matchedOutput = pure . ok $ HNil}
            _ -> NotMatched Other
    , toString = "/"
    }

-- | Endpoint that consumes the rest of the path reminder and decode it using provided DecodePath for some type @a@
paths ::
     forall a m. (DecodePath a, Applicative m, Typeable a)
  => Endpoint m [a]
paths =
  Endpoint
    { runEndpoint =
        \input@Input {..} ->
          Matched
            { matchedReminder = input {reminder = []}
            , matchedTrace = pathInfo request
            , matchedOutput = pure $ ok (map (decodePath @a) reminder >>= maybeToList)
            }
    , toString = "[" `T.append` T.pack (show $ typeRep (Proxy :: Proxy a)) `T.append` "]"
    }

-- | Endpoint that matches any path and discards reminder
pathAny :: (Applicative m) => Endpoint m (HList '[])
pathAny =
  Endpoint
    { runEndpoint =
        \input@Input {..} ->
          Matched
            {matchedReminder = input {reminder = []}, matchedTrace = pathInfo request, matchedOutput = pure . ok $ HNil}
    , toString = "*"
    }

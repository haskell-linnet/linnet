{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Linnet.Endpoints.Params
  ( param
  , paramMaybe
  , params
  , paramsNel
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8
import           Data.Either           (partitionEithers)
import           Data.List.NonEmpty    (NonEmpty (..), nonEmpty)
import           Linnet.Decode
import           Linnet.Endpoint
import           Linnet.Errors
import           Linnet.Output         (badRequest, ok)
import           Network.Wai           (queryString)

-- | Endpoint that tries to decode parameter @name@ from the request query string.
-- Always matches, but may fail with error in case:
-- * Parameter is not presented in request query
-- * There was a parameter decoding error
param ::
     forall a m. (DecodeEntity a, Applicative m)
  => B.ByteString
  -> Endpoint m a
param name =
  Endpoint
    { runEndpoint =
        \input ->
          let maybeParam = (lookup name . queryString . request) input
              output =
                case maybeParam of
                  Just (Just val) ->
                    case decodeEntity @a val of
                      Left err -> badRequest $ EntityNotParsed {notParsedEntityName = name, entityParsingError = err}
                      Right v -> ok v
                  _ -> badRequest $ MissingEntity name
           in Matched {matchedReminder = input, matchedOutput = pure output}
    , toString = "param " ++ C8.unpack name
    }

-- | Endpoint that tries to decode parameter @name@ from the request query string.
-- Always matches, but may fail with error in case:
-- * There was a parameter decoding error
paramMaybe ::
     forall a m. (DecodeEntity a, Applicative m)
  => B.ByteString
  -> Endpoint m (Maybe a)
paramMaybe name =
  Endpoint
    { runEndpoint =
        \input ->
          let maybeParam = (lookup name . queryString . request) input
              output =
                case maybeParam of
                  Just (Just val) ->
                    case decodeEntity @a val of
                      Left err -> badRequest $ EntityNotParsed {notParsedEntityName = name, entityParsingError = err}
                      Right v -> ok $ Just v
                  _ -> ok Nothing
           in Matched {matchedReminder = input, matchedOutput = pure output}
    , toString = "paramMaybe " ++ C8.unpack name
    }

-- | Endpoint that tries to decode all parameters @name@ from the request query string.
-- Always matches, but may fail with error in case:
-- * There was a parameter decoding error of at least one parameter value
params ::
     forall a m. (DecodeEntity a, Applicative m)
  => B.ByteString
  -> Endpoint m [a]
params name =
  Endpoint
    { runEndpoint =
        \input ->
          let filterParam = filter (\(paramName, _) -> paramName == name)
              ps =
                (filterParam . queryString . request) input >>=
                (\case
                   (k, Just v) -> [(k, v)]
                   _ -> [])
              (errors, values) = partitionEithers . map (decodeEntity @a . snd) $ ps
              output =
                case nonEmpty errors of
                  Just es -> badRequest $ LinnetErrors es
                  Nothing -> ok values
           in Matched {matchedReminder = input, matchedOutput = pure output}
    , toString = "params " ++ C8.unpack name
    }

-- | Endpoint that tries to decode all parameters @name@ from the request query string.
-- Always matches, but may fail with error in case:
-- * There was a parameter decoding error of at least one parameter value
-- * All parameters are empty or missing in request query
paramsNel ::
     forall a m. (DecodeEntity a, Monad m)
  => B.ByteString
  -> Endpoint m (NonEmpty a)
paramsNel name = mapOutput toNel $ params @a name
  where
    toNel []    = badRequest $ MissingEntity name
    toNel (h:t) = ok $ h :| t

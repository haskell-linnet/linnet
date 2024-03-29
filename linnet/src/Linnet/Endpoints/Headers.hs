{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Linnet.Endpoints.Headers
  ( header
  , headerMaybe
  ) where

import           Control.Monad.Catch     (MonadThrow, throwM)
import qualified Data.ByteString         as B
import qualified Data.CaseInsensitive    as CI
import           Data.Text               (append)
import qualified Data.Text.Encoding      as TE
import           Linnet.Decode
import           Linnet.Endpoint
import           Linnet.Endpoints.Entity
import           Linnet.Errors
import           Linnet.Input
import           Linnet.Output           (ok)
import           Network.Wai             (requestHeaders)

--required :: forall a m. (DecodeEntity a, Applicative m) => B.ByteString -> Endpoint m a
--required =
-- | Endpoint that tries to decode header @name@ from a request.
-- Always matches, but may throw an exception in case:
--
-- * Headers is not presented in the request
--
-- * There was a header decoding error
header ::
     forall a m. (DecodeEntity a, MonadThrow m)
  => B.ByteString
  -> Endpoint m a
header name =
  Endpoint
    { runEndpoint =
        \input ->
          let maybeHeader = (lookup (CI.mk name) . requestHeaders . request) input
              output =
                case maybeHeader of
                  Just val ->
                    case decodeEntity entity val of
                      Left err -> throwM err
                      Right v  -> return $ ok v
                  _ -> throwM $ MissingEntity entity
           in Matched {matchedReminder = input, matchedTrace = [], matchedOutput = output}
    , toString = "header " `append` TE.decodeUtf8 name
    }
  where
    entity = Header name

-- | Endpoint that tries to decode header @name@ from a request.
-- Always matches, but may throw an exception in case:
--
-- * There was a header decoding error
headerMaybe ::
     forall a m. (DecodeEntity a, MonadThrow m)
  => B.ByteString
  -> Endpoint m (Maybe a)
headerMaybe name =
  Endpoint
    { runEndpoint =
        \input ->
          let maybeHeader = (lookup (CI.mk name) . requestHeaders . request) input
              output =
                case maybeHeader of
                  Just val ->
                    case decodeEntity entity val of
                      Left err -> throwM err
                      Right v  -> return $ ok (Just v)
                  _ -> return $ ok Nothing
           in Matched {matchedReminder = input, matchedTrace = [], matchedOutput = output}
    , toString = "headerMaybe " `append` TE.decodeUtf8 name
    }
  where
    entity = Header name

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Linnet.Endpoints.Headers
  ( header
  , headerMaybe
  ) where

import           Control.Arrow         (right)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive  as CI
import           Linnet.Decode
import           Linnet.Endpoint
import           Linnet.Errors
import           Linnet.Input
import           Linnet.Output         (Output, badRequest, ok)
import           Network.Wai           (requestHeaders)
import Control.Monad.Catch (MonadThrow, throwM)

--required :: forall a m. (DecodeEntity a, Applicative m) => B.ByteString -> Endpoint m a
--required =
-- | Endpoint that tries to decode header @name@ from a request.
-- | Always matches, but may fail with error in case:
-- * Headers is not presented in the request
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
                    case decodeEntity @a val of
                      Left err -> throwM $ EntityNotParsed {notParsedEntityName = name, entityParsingError = err}
                      Right v -> return $ ok v
                  _ -> throwM $ MissingEntity name
           in Matched {matchedReminder = input, matchedOutput = output}
    , toString = "header " ++ C8.unpack name
    }

-- | Endpoint that tries to decode header @name@ from a request.
-- | Always matches, but may fail with error in case:
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
                    case decodeEntity @a val of
                      Left err -> throwM $ EntityNotParsed {notParsedEntityName = name, entityParsingError = err}
                      Right v -> return $ ok (Just v)
                  _ -> return $ ok Nothing
           in Matched {matchedReminder = input, matchedOutput = output}
    , toString = "headerMaybe " ++ C8.unpack name
    }

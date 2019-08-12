{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Linnet.Endpoints.Bodies
  ( body
  , bodyMaybe
  , jsonBody
  , jsonBodyMaybe
  , textBody
  , textBodyMaybe
  ) where

import           Control.Monad.Catch     (MonadThrow, throwM)
import           Control.Monad.IO.Class  (MonadIO (..))
import qualified Data.ByteString.Lazy    as BL
import           Linnet.ContentTypes     (ApplicationJson, TextPlain)
import           Linnet.Decode
import           Linnet.Endpoint
import           Linnet.Endpoints.Entity
import           Linnet.Errors
import           Linnet.Input
import           Linnet.Output
import           Network.Wai             (RequestBodyLength (..),
                                          lazyRequestBody, requestBodyLength)

decodeBody ::
     forall ct a m. (Decode ct a, MonadThrow m)
  => BL.ByteString
  -> m (Output a)
decodeBody payload =
  case decode @ct @a payload of
    Right a -> pure $ ok a
    Left e  -> throwM $ EntityNotParsed Body e

-- | Endpoint that tries to decode body of request into some type @a@ using corresponding 'Decode' instance
-- Always matches, but may throw an exception in case:
--
--    * Content length header was missing, chunked or equal to 0
--
--    * There was a body decoding error
body ::
     forall ct a m. (Decode ct a, MonadIO m, MonadThrow m)
  => Endpoint m a
body =
  Endpoint
    { runEndpoint =
        \input ->
          let mOut =
                case (requestBodyLength . request) input of
                  KnownLength 0 -> throwM $ MissingEntity Body
                  ChunkedBody -> throwM $ MissingEntity Body
                  KnownLength _ -> (liftIO . lazyRequestBody . request) input >>= decodeBody @ct @a
           in Matched {matchedReminder = input, matchedOutput = mOut}
    , toString = "body"
    }
  where
    entity = Body

-- | Endpoint that tries to decode body of request into some type @a@ using corresponding 'Decode' instance
-- Always matches, but may throw an exception in case:
--
--    * There was a body decoding error
bodyMaybe ::
     forall ct a m. (Decode ct a, MonadIO m, MonadThrow m)
  => Endpoint m (Maybe a)
bodyMaybe =
  Endpoint
    { runEndpoint =
        \input ->
          let mOut =
                case (requestBodyLength . request) input of
                  KnownLength 0 -> pure $ ok Nothing
                  ChunkedBody -> pure $ ok Nothing
                  KnownLength _ -> (fmap . fmap) Just ((liftIO . lazyRequestBody . request) input >>= decodeBody @ct @a)
           in Matched {matchedReminder = input, matchedOutput = mOut}
    , toString = "bodyMaybe"
    }

-- | Alias for @body \@TextPlain@
textBody :: (Decode TextPlain a, MonadIO m, MonadThrow m) => Endpoint m a
textBody = body @TextPlain

-- | Alias for @bodyMaybe \@TextPlain@
textBodyMaybe :: (Decode TextPlain a, MonadIO m, MonadThrow m) => Endpoint m (Maybe a)
textBodyMaybe = bodyMaybe @TextPlain

-- | Alias for @body \@ApplicationJson@
jsonBody :: (Decode ApplicationJson a, MonadIO m, MonadThrow m) => Endpoint m a
jsonBody = body @ApplicationJson

-- | Alias for @bodyMaybe \@ApplicationJson@
jsonBodyMaybe :: (Decode ApplicationJson a, MonadIO m, MonadThrow m) => Endpoint m (Maybe a)
jsonBodyMaybe = bodyMaybe @ApplicationJson

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
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

import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Lazy   as BL
import           Linnet.ContentTypes    (ApplicationJson, TextPlain)
import           Linnet.Decode
import           Linnet.Endpoint
import           Linnet.Errors
import           Linnet.Output
import           Network.Wai            (RequestBodyLength (..),
                                         lazyRequestBody, requestBodyLength)

decodeBody ::
     forall ct a. (Decode ct a)
  => BL.ByteString
  -> Output a
decodeBody =
  (\case
     Right a -> ok a
     Left e -> badRequest e) .
  decode @ct @a

-- | Endpoint that tries to decode body of request into some type @a@ using corresponding 'Decode' instance
-- Always matches, but may fail with error in case:
--
--    * Content length header was missing, chunked or equal to 0
--
--    * There was a body decoding error
body ::
     forall ct a m. (Decode ct a, MonadIO m)
  => Endpoint m a
body =
  Endpoint
    { runEndpoint =
        \input ->
          let mOut =
                case (requestBodyLength . request) input of
                  KnownLength 0 -> pure $ badRequest $ MissingEntity "body"
                  ChunkedBody -> pure $ badRequest $ MissingEntity "body"
                  KnownLength _ -> decodeBody @ct @a <$> (liftIO . lazyRequestBody . request) input
           in Matched {matchedReminder = input, matchedOutput = mOut}
    , toString = "body"
    }

-- | Endpoint that tries to decode body of request into some type @a@ using corresponding 'Decode' instance
-- Always matches, but may fail with error in case:
-- 
--    * There was a body decoding error
bodyMaybe ::
     forall ct a m. (Decode ct a, MonadIO m)
  => Endpoint m (Maybe a)
bodyMaybe =
  Endpoint
    { runEndpoint =
        \input ->
          let mOut =
                case (requestBodyLength . request) input of
                  KnownLength 0 -> pure $ ok Nothing
                  ChunkedBody -> pure $ ok Nothing
                  KnownLength _ -> (fmap . fmap) Just $ decodeBody @ct @a <$> (liftIO . lazyRequestBody . request) input
           in Matched {matchedReminder = input, matchedOutput = mOut}
    , toString = "bodyMaybe"
    }

-- | Alias for body @TextPlain
textBody :: (Decode TextPlain a, MonadIO m) => Endpoint m a
textBody = body @TextPlain

-- | Alias for bodyMaybe @TextPlain
textBodyMaybe :: (Decode TextPlain a, MonadIO m) => Endpoint m (Maybe a)
textBodyMaybe = bodyMaybe @TextPlain

-- | Alias for body @ApplicationJson
jsonBody :: (Decode ApplicationJson a, MonadIO m) => Endpoint m a
jsonBody = body @ApplicationJson

-- | Alias for bodyMaybe @ApplicationJson
jsonBodyMaybe :: (Decode ApplicationJson a, MonadIO m) => Endpoint m (Maybe a)
jsonBodyMaybe = bodyMaybe @ApplicationJson

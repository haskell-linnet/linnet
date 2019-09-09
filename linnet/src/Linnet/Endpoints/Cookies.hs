{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Linnet.Endpoints.Cookies
  ( cookie
  , cookieMaybe
  ) where

import           Control.Monad.Catch     (MonadThrow, throwM)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as C8
import           Data.Text               (append)
import qualified Data.Text.Encoding      as TE
import           Linnet.Decode
import           Linnet.Endpoint
import           Linnet.Endpoints.Entity
import           Linnet.Errors
import           Linnet.Input
import           Linnet.Output
import           Network.HTTP.Types      (hCookie)
import           Network.URI.Encode      (decodeByteString)
import           Network.Wai             (requestHeaders)

findCookie :: B.ByteString -> B.ByteString -> Maybe B.ByteString
findCookie name cookies =
  lookup name $
  C8.split ';' cookies >>=
  (\pairs ->
     case C8.split '=' pairs of
       [k, v] -> [(k, decodeByteString v)]
       _      -> [])

-- | Endpoint that tries to decode cookie @name@ from a request.
-- Always matches, but may throw an exception in case:
--
-- * Cookie is not presented in the request
--
-- * There was a cookie decoding error
cookie ::
     forall a m. (DecodeEntity a, MonadThrow m)
  => B.ByteString
  -> Endpoint m a
cookie name =
  Endpoint
    { runEndpoint =
        \input ->
          let maybeCookie = (lookup hCookie . requestHeaders . request) input >>= findCookie name
              output =
                case maybeCookie of
                  Just val ->
                    case decodeEntity entity val of
                      Left err -> throwM err
                      Right v  -> return $ ok v
                  _ -> throwM $ MissingEntity entity
           in Matched {matchedReminder = input, matchedTrace = [], matchedOutput = output}
    , toString = "cookie " `append` TE.decodeUtf8 name
    }
  where
    entity = Cookie name

-- | Endpoint that tries to decode cookie @name@ from a request.
-- Always matches, but may throw an exception in case:
--
-- * There was a cookie decoding error
cookieMaybe ::
     forall a m. (DecodeEntity a, MonadThrow m)
  => B.ByteString
  -> Endpoint m (Maybe a)
cookieMaybe name =
  Endpoint
    { runEndpoint =
        \input ->
          let maybeCookie = (lookup hCookie . requestHeaders . request) input >>= findCookie name
              output =
                case maybeCookie of
                  Just val ->
                    case decodeEntity entity val of
                      Left err -> throwM err
                      Right v  -> return $ ok (Just v)
                  _ -> return $ ok Nothing
           in Matched {matchedReminder = input, matchedTrace = [], matchedOutput = output}
    , toString = "cookieMaybe " `append` TE.decodeUtf8 name
    }
  where
    entity = Header name

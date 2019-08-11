{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Linnet.Endpoints.Cookies
  ( cookie
  , cookieMaybe
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive  as CI
import           Linnet.Decode
import           Linnet.Endpoint
import           Linnet.Errors
import           Linnet.Output
import           Network.Wai           (requestHeaders)

findCookie :: B.ByteString -> B.ByteString -> Maybe B.ByteString
findCookie name cookies =
  lookup name $
  C8.split ';' cookies >>=
  (\pairs ->
     case C8.split '=' pairs of
       [k, v] -> [(k, v)]
       _      -> [])

-- | Endpoint that tries to decode cookie @name@ from a request.
-- | Always matches, but may fail with error in case:
-- * Cookie is not presented in the request
-- * There was a cookie decoding error
cookie ::
     forall a m. (DecodeEntity a, Applicative m)
  => B.ByteString
  -> Endpoint m a
cookie name =
  Endpoint
    { runEndpoint =
        \input ->
          let maybeCookie = (lookup (CI.mk "Cookie") . requestHeaders . request) input >>= findCookie name
              output =
                case maybeCookie of
                  Just val ->
                    case decodeEntity @a val of
                      Left err -> badRequest $ EntityNotParsed {notParsedEntityName = name, entityParsingError = err}
                      Right v -> ok v
                  _ -> badRequest $ MissingEntity name
           in Matched {matchedReminder = input, matchedOutput = pure output}
    , toString = "cookie " ++ C8.unpack name
    }

-- | Endpoint that tries to decode cookie @name@ from a request.
-- | Always matches, but may fail with error in case:
-- * There was a cookie decoding error
cookieMaybe ::
     forall a m. (DecodeEntity a, Applicative m)
  => B.ByteString
  -> Endpoint m (Maybe a)
cookieMaybe name =
  Endpoint
    { runEndpoint =
        \input ->
          let maybeCookie = (lookup (CI.mk "Cookie") . requestHeaders . request) input >>= findCookie name
              output =
                case maybeCookie of
                  Just val ->
                    case decodeEntity @a val of
                      Left err -> badRequest $ EntityNotParsed {notParsedEntityName = name, entityParsingError = err}
                      Right v -> ok $ Just v
                  _ -> ok Nothing
           in Matched {matchedReminder = input, matchedOutput = pure output}
    , toString = "cookieMaybe " ++ C8.unpack name
    }

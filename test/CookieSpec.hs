{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module CookieSpec
  ( spec
  ) where

import           Control.Exception          (SomeException, toException)
import qualified Data.ByteString            as BS
import           Data.ByteString.Conversion (ToByteString, toByteString')
import qualified Data.CaseInsensitive       as CI
import qualified Data.Text                  as T
import           EntityEndpointLaws
import           Instances
import           Linnet
import           Linnet.Endpoints.Entity
import           Linnet.Errors
import           Linnet.Input
import           Network.Wai                (Request (..))
import           Test.Hspec
import           Util
import Network.URI.Encode (encodeByteString)

withCookie :: (ToByteString a) => BS.ByteString -> a -> Input
withCookie key value =
  let i = inputGet "/" []
      req = request i
      headers = requestHeaders req
   in i
        { request =
            req
              { requestHeaders =
                  headers ++ [(CI.mk "Cookie", key `BS.append` "=" `BS.append` encodeByteString (toByteString' value))]
              }
        }

spec :: Spec
spec = do
  checkLaws "Text" $ entityEndpointLaws @T.Text @(Either SomeException) (cookie "x") (withCookie "x")
  checkLaws "Int" $ entityEndpointLaws @Int @(Either SomeException) (cookie "x") (withCookie "x")
  checkLaws "Double" $ entityEndpointLaws @Double @(Either SomeException) (cookie "x") (withCookie "x")
  checkLaws "Float" $ entityEndpointLaws @Float @(Either SomeException) (cookie "x") (withCookie "x")
  it "throws an error if cookie is missing" $ do
    let e = cookie @BS.ByteString @IO "foo"
    result <- resultOutputEither (runEndpoint e (inputGet "/" []))
    result `shouldBe` (Left $ toException (MissingEntity (Cookie "foo")))
  it "throws an error if header is malformed" $ do
    let e = cookie @Int @IO "foo"
    result <- resultOutputEither (runEndpoint e (withCookie "foo" ("bar" :: BS.ByteString)))
    result `shouldBe` (Left $ toException (EntityNotParsed (Cookie "foo") (DecodeError "Failed reading: Invalid Int")))
  it "returns nothing if cookie is not required" $ do
    let e = cookieMaybe @BS.ByteString @IO "foo"
    result <- resultOutputEither (runEndpoint e (inputGet "/" []))
    result `shouldBe` (Right $ Just $ ok Nothing)

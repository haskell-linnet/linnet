{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HeaderSpec
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

withHeader :: (ToByteString a) => BS.ByteString -> a -> Input
withHeader key value =
  let i = inputFromGet "/" []
      req = request i
      headers = requestHeaders req
   in i {request = req {requestHeaders = headers ++ [(CI.mk key, toByteString' value)]}}

spec :: Spec
spec = do
  checkLaws "ByteString" $ entityEndpointLaws @BS.ByteString @(Either SomeException) (header "x") (withHeader "x")
  checkLaws "Text" $ entityEndpointLaws @T.Text @(Either SomeException) (header "x") (withHeader "x")
  checkLaws "Int" $ entityEndpointLaws @Int @(Either SomeException) (header "x") (withHeader "x")
  checkLaws "Double" $ entityEndpointLaws @Double @(Either SomeException) (header "x") (withHeader "x")
  checkLaws "Float" $ entityEndpointLaws @Float @(Either SomeException) (header "x") (withHeader "x")
  it "throws an error if header is missing" $ do
    let e = header @BS.ByteString @IO "foo"
    result <- resultOutputEither (runEndpoint e (inputFromGet "/" []))
    result `shouldBe` (Left $ toException (MissingEntity (Header "foo")))
  it "throws an error if header is malformed" $ do
    let e = header @Int @IO "foo"
    result <- resultOutputEither (runEndpoint e (withHeader "foo" ("bar" :: BS.ByteString)))
    result `shouldBe` (Left $ toException (EntityNotParsed (Header "foo") (DecodeError "Failed reading: Invalid Int")))
  it "returns nothing if cookie is not required" $ do
    let e = headerMaybe @BS.ByteString @IO "foo"
    result <- resultOutputEither (runEndpoint e (inputFromGet "/" []))
    result `shouldBe` (Right $ Just $ ok Nothing)

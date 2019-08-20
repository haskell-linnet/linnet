{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module ParamSpec where

import           Control.Exception          (SomeException, fromException,
                                             toException)
import qualified Data.ByteString            as BS
import           Data.ByteString.Conversion (ToByteString, toByteString')
import           Data.Functor.Identity      (Identity)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.Text                  as T
import           EntityEndpointLaws
import           Instances
import           Linnet
import           Linnet.Endpoints.Entity
import           Linnet.Errors
import           Linnet.Input
import           Test.Hspec
import           Util

withParam :: (ToByteString a) => BS.ByteString -> a -> Input
withParam key a = inputGet "/" [(key, Just $ toByteString' a)]

spec :: Spec
spec = do
  checkLaws "ByteString" $ entityEndpointLaws @BS.ByteString @(Either SomeException) (param "x") (withParam "x")
  checkLaws "Text" $ entityEndpointLaws @T.Text @(Either SomeException) (param "x") (withParam "x")
  checkLaws "Int" $ entityEndpointLaws @Int @(Either SomeException) (param "x") (withParam "x")
  checkLaws "Double" $ entityEndpointLaws @Double @(Either SomeException) (param "x") (withParam "x")
  checkLaws "Float" $ entityEndpointLaws @Float @(Either SomeException) (param "x") (withParam "x")
  it "throws an error if param is missing" $ do
    let e = param @BS.ByteString @IO "foo"
    result <- resultOutputEither (runEndpoint e (inputGet "/" []))
    result `shouldBe` (Left $ toException (MissingEntity (Param "foo")))
  it "throws an error if param is malformed" $ do
    let e = param @Int @IO "foo"
    result <- resultOutputEither (runEndpoint e (inputGet "/" [("foo", Just "bar")]))
    result `shouldBe` (Left $ toException (EntityNotParsed (Param "foo") (DecodeError "Failed reading: Invalid Int")))
  it "collect errors in params & paramsNel" $ do
    let ps = params @Int @IO "test"
    let psNel = paramsNel @Int @IO "test"
    let exception = LinnetErrors $ EntityNotParsed (Param "test") (DecodeError "Failed reading: Invalid Int") :| []
    let i = inputGet "/" [("test", Just "foo")]
    result <- resultOutputEither (runEndpoint ps i)
    result `shouldBe` (Left $ toException exception)
    result' <- resultOutputEither (runEndpoint psNel i)
    result' `shouldBe` (Left $ toException exception)
  it "returns nothing if param is not required" $ do
    let e = paramMaybe @BS.ByteString @IO "foo"
    result <- resultOutputEither (runEndpoint e (inputGet "/" []))
    result `shouldBe` (Right $ Just $ ok Nothing)

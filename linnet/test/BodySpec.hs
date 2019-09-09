{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module BodySpec
  ( spec
  ) where

import           Control.Concurrent         (MVar, newMVar, putMVar, readMVar,
                                             tryTakeMVar)
import           Control.Exception          (SomeException, toException)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString            as BS
import           Data.ByteString.Conversion (ToByteString, toByteString')
import qualified Data.CaseInsensitive       as CI
import qualified Data.Text                  as T
import           Debug.Trace                (trace)
import           EntityEndpointLaws
import           Instances
import           Linnet
import           Linnet.Endpoint            (EndpointResult (..))
import           Linnet.Endpoints.Entity
import           Linnet.Errors
import           Linnet.Input
import           Network.Wai                (Request (..),
                                             RequestBodyLength (..))
import           Test.Hspec
import           Test.QuickCheck            (property)
import           Test.QuickCheck.Monadic    (assert, monadicIO)
import           Util

withBody :: (ToByteString a) => MVar a -> Input
withBody mvar =
  let i = inputGet "/" []
      req = request i
      mBody = do
        result <- tryTakeMVar mvar
        return
          (case result of
             Just value -> toByteString' value
             Nothing    -> mempty)
   in i {request = req {requestBody = mBody, requestBodyLength = KnownLength 123}}

withEmptyBody :: Input
withEmptyBody =
  let i = inputGet "/" []
      req = request i
   in i {request = req {requestBody = pure mempty, requestBodyLength = KnownLength 0}}

spec :: Spec
spec = do
  it "decodes body correctly" $
    property $ \(foo :: Foo) ->
      monadicIO $ do
        let e = textBody @Foo @IO
        mvar <- liftIO $ newMVar (show foo)
        result <- liftIO $ resultOutputEither (runEndpoint e (withBody mvar))
        assert $ result == (Right $ Just (ok foo))
  it "decodes optional body correctly" $
    property $ \(foo :: Foo) ->
      monadicIO $ do
        let e = textBodyMaybe @Foo @IO
        mvar <- liftIO $ newMVar (show foo)
        result <- liftIO $ resultOutputEither (runEndpoint e (withBody mvar))
        assert $ result == (Right $ Just (ok (Just foo)))
  it "throws an error if body is missing" $ do
    let e = textBody @Foo @IO
    result <- resultOutputEither (runEndpoint e (inputGet "/" []))
    result `shouldBe` (Left $ toException (MissingEntity Body))
  it "throws an error if body is malformed" $ do
    let e = textBody @Foo @IO
    mvar <- newMVar ("foo" :: T.Text)
    result <- resultOutputEither (runEndpoint e (withBody mvar))
    result `shouldBe` (Left $ toException (EntityNotParsed Body (DecodeError "Couldn't parse Foo")))
  it "returns nothing if body is not required" $ do
    let e = textBodyMaybe @Foo @IO
    result <- resultOutputEither (runEndpoint e withEmptyBody)
    result `shouldBe` (Right $ Just $ ok Nothing)
  it "returns no trace" $
    property $ \(foo :: Foo) ->
      monadicIO $ do
        let e = textBody @Foo @IO
        mvar <- liftIO $ newMVar (show foo)
        let Matched {..} = runEndpoint e (withBody mvar)
        assert $ null matchedTrace

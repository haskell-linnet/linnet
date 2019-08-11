{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module EndpointSpec
  ( spec
  ) where

import           Control.Exception       (SomeException)
import qualified Data.ByteString         as B
import           Data.Functor.Identity
import           Data.List               (uncons)
import           Data.Maybe              (isNothing, maybeToList)
import qualified Data.Text               as T
import           Instances
import           Linnet
import           Linnet.Endpoint
import           Linnet.Internal.HList
import           Linnet.Output           (withHeader)
import           Network.HTTP.Types      (methodConnect, methodDelete,
                                          methodGet, methodHead, methodOptions,
                                          methodPatch, methodPost, methodPut,
                                          methodTrace)
import           Network.Wai             (requestMethod)
import           Test.Hspec
import           Test.QuickCheck         (conjoin, property)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)
import           Util

spec :: Spec
spec =
  describe "Endpoint" $ do
    it "supports simple fmap" $
      property $ \(i :: Input) ->
        resultValueUnsafe (runEndpoint (T.toUpper <$> path @T.Text) i) ==
        Identity (fmap T.toUpper . headOption $ reminder i)
    it "supports transformOutput" $
      property $ \(e :: Endpoint (Either SomeException) String) ->
        monadicIO $ do
          eq <- run $ compareEndpoints (transformOutput ((fmap . fmap) length) e) (fmap length e)
          assert eq
    it "supports transform" $
      property $ \(e :: Endpoint (Either SomeException) String) ->
        monadicIO $ do
          eq <- run $ compareEndpoints (transform (fmap length) e) (fmap length e)
          assert eq
    it "propagates default (Ok) output" $
      property $ \(i :: Input) ->
        resultOutputUnsafe (runEndpoint (path @T.Text) i) == Identity (fmap ok . headOption $ reminder i)
    it "propagates default (Ok) output through fmap, mapM'" $
      property $ \(i :: Input) ->
        let expected = ok . T.length <$> (headOption . reminder) i
            endpoint = path @T.Text
         in resultOutputUnsafe (runEndpoint (fmap T.length endpoint) i) == Identity expected &&
            resultOutputUnsafe (runEndpoint (mapM' (pure . T.length) endpoint) i) == Identity expected
    it "propagates output through mapOutputM" $
      property $ \(i :: Input) ->
        let expected i = withHeader ("Foo", "Bar") $ created i
         in resultOutputUnsafe (runEndpoint (mapOutputM (pure . expected . T.length) $ path @T.Text) i) ==
            Identity (fmap (expected . T.length) (headOption $ reminder i))
    it "matches one path segment" $
      property $ \(i :: Input) ->
        let v = (maybeToList . headOption . reminder $ i) >>= (\s -> [matchedReminder $ runEndpoint (p' @Identity s) i])
         in null v || v == [i {reminder = tail . reminder $ i}]
    it "matches the entire input with pathAny" $
      property $ \(i :: Input) -> matchedReminder (runEndpoint (pathAny @Identity) i) == i {reminder = []}
    it "matches empty path" $
      property $ \(i :: Input) ->
        let result = runEndpoint (pathEmpty @Identity) i
         in (null (reminder i) && isMatched result) || (reminder i /= [] && not (isMatched result))
    it "matches the HTTP method" $
      let matchMethod ::
               B.ByteString -> (Endpoint Identity (HList '[]) -> Endpoint Identity (HList '[])) -> Input -> Bool
          matchMethod method f input =
            let result = runEndpoint (f zero) input
             in ((requestMethod . request) input == method && maybeReminder result == Just input) ||
                ((requestMethod . request) input /= method && isNothing (maybeReminder result))
       in conjoin
            [ property $ matchMethod methodGet get
            , property $ matchMethod methodPost post
            , property $ matchMethod methodPut put
            , property $ matchMethod methodPatch patch
            , property $ matchMethod methodDelete delete
            , property $ matchMethod methodHead head'
            , property $ matchMethod methodOptions options
            , property $ matchMethod methodConnect connect
            , property $ matchMethod methodTrace trace'
            ]
    it "always match with identity instance" $
      property $ \(i :: Input) -> maybeReminder (runEndpoint (zero @Identity) i) == Just i

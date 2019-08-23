{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module EndpointSpec
  ( spec
  ) where

import           Control.Applicative       ((<|>))
import           Control.Exception         (SomeException, fromException,
                                            toException)
import           Control.Monad.Catch       (throwM)
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.ByteString           as B
import           Data.Either               (isLeft, lefts)
import           Data.Function             ((&))
import           Data.Functor.Identity
import           Data.List                 (uncons)
import           Data.List.NonEmpty        (NonEmpty (..), toList)
import           Data.Maybe                (isNothing, maybeToList)
import qualified Data.Text                 as T
import           Debug.Trace               (trace)
import           ExtractPathLaws
import           Instances
import           Linnet
import           Linnet.Endpoint
import           Linnet.Endpoints.Entity
import           Linnet.Errors
import           Linnet.Input
import           Linnet.Internal.Coproduct (Coproduct (..))
import           Linnet.Internal.HList
import           Linnet.Output             (withHeader)
import           Network.HTTP.Types        (methodConnect, methodDelete,
                                            methodGet, methodHead,
                                            methodOptions, methodPatch,
                                            methodPost, methodPut, methodTrace)
import           Network.Wai               (requestMethod)
import           Test.Hspec
import           Test.QuickCheck           (conjoin, property)
import           Test.QuickCheck.Monadic   (assert, monadicIO, run)
import           Util

spec :: Spec
spec = do
  checkLaws "Text" $ extractPathLaws @T.Text
  checkLaws "Int" $ extractPathLaws @Int
  it "supports simple fmap" $
    property $ \(i :: Input) ->
      resultValueUnsafe (runEndpoint (T.toUpper <$> path @T.Text) i) ==
      Identity (fmap T.toUpper . headOption $ reminder i)
  it "supports transformOutput" $
    property $ \(e :: Endpoint (Either SomeException) String) ->
      transformOutput ((fmap . fmap) length) e == fmap length e
  it "supports transform" $
    property $ \(e :: Endpoint (Either SomeException) String) -> transform (fmap length) e == fmap length e
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
    let matchMethod :: B.ByteString -> (Endpoint Identity (HList '[]) -> Endpoint Identity (HList '[])) -> Input -> Bool
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
  it "always matches with identity instance" $
    property $ \(i :: Input) -> maybeReminder (runEndpoint (zero @Identity) i) == Just i
  it "matches & consumes the entire input" $
    property $ \(i :: Input) ->
      let e = foldl (//) (zero @(Either SomeException)) $ map p' (reminder i)
       in maybeReminder (runEndpoint e i) == Just (i {reminder = []})
  it "shouldn't match if one of the endpoints has failed in (//) composition" $
    property $ \(i :: Input, s :: T.Text) ->
      isNothing (maybeReminder (runEndpoint (pathAny @(Either SomeException) // p' s) i))
  it "matches if at least one of the endpoints succeed in alternative <|>" $
    let matchOneOfTwo :: (T.Text -> Endpoint IO (HList '[])) -> Input -> Bool
        matchOneOfTwo f input = isNothing v || v == Just input {reminder = tail $ reminder input}
          where
            v = f <$> (headOption . reminder) input >>= (\e -> maybeReminder (runEndpoint e input))
     in conjoin
          [ property $ matchOneOfTwo (\s -> p' s <|> p' (T.reverse s))
          , property $ matchOneOfTwo (\s -> p' (T.reverse s) <|> p' s)
          ]
  it "always responds with the same output if it's constant" $
    conjoin
      [ property $ \(t :: T.Text) ->
          monadicIO $ do
            p <- liftIO $ resultValueUnsafe (runEndpoint (pure t) (inputGet "/" []))
            l <- liftIO $ resultValueUnsafe (runEndpoint (lift $ pure t) (inputGet "/" []))
            assert (p == Just t)
            assert (l == Just t)
      , property $ \(out :: Output T.Text) ->
          resultOutputUnsafe (runEndpoint (liftOutputM $ pure out) (inputGet "/" [])) == Identity (Just out)
      ]
  it "handles the exception raised in a monad" $
    property $ \(i :: Input, s :: T.Text) ->
      monadicIO $ do
        let e = lift (throwM @IO (TestException "test")) & handle (\(e :: TestException) -> return $ created s)
        result <- liftIO $ runEndpoint e i & resultOutputEither
        assert (result == Right (Just (created s)))
  it "re-raises the exception if it wasn't handled" $
    property $ \(i :: Input, s :: T.Text) ->
      monadicIO $ do
        let e :: Endpoint IO T.Text = lift (throwM @IO (TestException "test"))
        result <- liftIO $ runEndpoint e i & resultOutputEither
        assert (result == Left (toException $ TestException "test"))
  it "throws MissingEntity if an item wasn't found" $ do
    let i = inputGet "/" []
    let name = "test"
    let (endpoints, exceptions) =
          unzip
            [ (param @T.Text name, MissingEntity $ Param name)
            , (T.intercalate ";" . toList <$> paramsNel @T.Text name, MissingEntity $ Param name)
            , (header @T.Text name, MissingEntity $ Header name)
            , (cookie @T.Text name, MissingEntity $ Cookie name)
            , (textBody @T.Text, MissingEntity Body)
            ]
    results <- lefts <$> mapM (\e -> runEndpoint e i & resultOutputEither) endpoints
    map fromException results `shouldBe` Just <$> exceptions
  it "accumulates Linnet errors in product" $
    property $ \(i :: Input, e :: LinnetError, e' :: LinnetError) ->
      monadicIO $ do
        let ea = lift @IO @Int (throwM e)
        let eb = lift @IO @Int (throwM e')
        result <- liftIO $ resultOutputEither (runEndpoint (ea // eb) i)
        result' <- liftIO $ resultOutputEither (runEndpoint (eb // ea) i)
        assert (result == (Left $ toException (e <> e')))
        assert (result' == (Left $ toException (e' <> e)))
  it "fails fast once non-Linnet error is observed" $
    property $ \(i :: Input, e :: LinnetError) ->
      monadicIO $ do
        let exception = TestException "foo"
        let ea = lift @IO @Int (throwM e)
        let eb = lift @IO @Int (throwM exception)
        result <- liftIO $ resultOutputEither (runEndpoint (ea // eb) i)
        result' <- liftIO $ resultOutputEither (runEndpoint (eb // ea) i)
        assert (result == (Left $ toException exception))
        assert (result' == (Left $ toException exception))
  it "selects endpoint with the least reminder" $
    property $ \(i :: Input, s :: T.Text) ->
      monadicIO $ do
        let e1 = foldl (\acc segment -> acc // p' segment) zero (reminder i) ~>> return (ok True)
        let e2 = e1 // p' s ~>> (\t -> return $ ok (5 :: Int))
        let composition = e1 |+| e2
        let composition' = e2 |+| e1
        let newInput = i {reminder = reminder i ++ [s]}
        let endpointResult1 = runEndpoint e1 newInput
        let endpointResult2 = runEndpoint e2 newInput
        assert $ isMatched endpointResult1
        assert $ isMatched endpointResult2
        result <- liftIO $ resultValueUnsafe (runEndpoint composition newInput)
        result' <- liftIO $ resultValueUnsafe (runEndpoint composition' newInput)
        assert (result == Just (Inr (Inl 5)))
        assert (result' == Just (Inl 5))
  it "returns request in root endpoint" $
    property $ \(i :: Input) -> resultValueUnsafe (runEndpoint root i) == Identity (Just $ request i)
  it "adjoins endpoints together in //" $ do
    let i = inputGet "foo/bar/zoo" []
    let endpoint = (path @T.Text // path @T.Text // path @T.Text) ~>> (\p1 p2 p3 -> return $ ok (p1, p2, p3))
    result <- liftIO $ resultValueUnsafe $ runEndpoint endpoint i
    result `shouldBe` Just ("foo", "bar", "zoo")
  it "catches exception with try" $
    property $ \(i :: Input) ->
      monadicIO $ do
        let exception = TestException "foo"
        let endpoint = (lift $ throwM exception) :: Endpoint IO Int
        let handled = try endpoint
        result <- liftIO $ resultValueUnsafe (runEndpoint handled i)
        assert $ result == Just (Left exception)

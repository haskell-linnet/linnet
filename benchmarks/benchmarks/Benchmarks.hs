{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import           Criterion.Main

import           Control.Applicative   (empty, (<|>))
import           Control.DeepSeq       (NFData)
import           Control.Exception     (SomeException)
import           Control.Monad         (replicateM)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Function         ((&))
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.IORef            (IORef, atomicModifyIORef, newIORef)
import           Data.Text             (Text)
import           Foo
import           Linnet
import           Linnet.Aeson
import           Linnet.Endpoint
import           Linnet.Input
import           Network.Wai           (RequestBodyLength (..), defaultRequest,
                                        pathInfo, requestBody,
                                        requestBodyLength)
import           Network.Wai.Internal  (ResponseReceived (..))
import           System.Random         (randomIO)

main =
  defaultMain
    [ bodyBenchmarks
    , matchPathBenchmarks
    , extractPathBenchmarks
    , productBenchmarks
    , alternativeBenchmarks
    , mapBenchmarks
    , bootstrapBenchmarks
    ]

bodyBenchmarks :: Benchmark
bodyBenchmarks = bgroup "body" [fooMaybe, foo, byteStringMaybe, byteString, byteStringEmpty]
  where
    fooMaybe = bench "fooMaybe" $ nfIO (postPayload >>= result . runEndpoint (bodyMaybe @TextPlain @Foo @IO))
    foo = bench "foo" $ nfIO (postPayload >>= result . runEndpoint (body @TextPlain @Foo @IO))
    byteStringMaybe =
      bench "byteStringMaybe" $ nfIO (postPayload >>= result . runEndpoint (bodyMaybe @TextPlain @BS.ByteString @IO))
    byteString = bench "byteString" $ nfIO (postPayload >>= result . runEndpoint (body @TextPlain @BS.ByteString @IO))
    byteStringEmpty =
      bench "byteStringEmpty" $
      nfIO ((result . runEndpoint (bodyMaybe @TextPlain @BS.ByteString @IO)) $ inputFromRequest defaultRequest)

matchPathBenchmarks :: Benchmark
matchPathBenchmarks = bgroup "matchPath" [matched, notMatched]
  where
    Endpoint {..} = p' "foo"
    matched = bench "matched" $ whnf (runIdentity . result . runEndpoint) getFooBarBaz
    notMatched = bench "notMatched" $ whnf (runIdentity . result . runEndpoint) getRoot

extractPathBenchmarks :: Benchmark
extractPathBenchmarks =
  bgroup
    "extractPath"
    [textJust, textNothing, byteStringJust, byteStringNothing, intJust, intNothing, integerJust, integerNothing]
  where
    textJust = bench "textJust" $ nf (runIdentity . result . runEndpoint (path @Text)) getFooBarBaz
    textNothing = bench "textNothing" $ nf (runIdentity . result . runEndpoint (path @Text)) getRoot
    byteStringJust = bench "byteStringJust" $ nf (runIdentity . result . runEndpoint (path @BS.ByteString)) getFooBarBaz
    byteStringNothing =
      bench "byteStringNothing" $ nf (runIdentity . result . runEndpoint (path @BS.ByteString)) getRoot
    intJust = bench "intJust" $ nf (runIdentity . result . runEndpoint (path @Int)) getTenTwenty
    intNothing = bench "intNothing" $ nf (runIdentity . result . runEndpoint (path @Int)) getRoot
    integerJust = bench "integerJust" $ nf (runIdentity . result . runEndpoint (path @Integer)) getTenTwenty
    integerNothing = bench "integerNothing" $ nf (runIdentity . result . runEndpoint (path @Integer)) getRoot

productBenchmarks :: Benchmark
productBenchmarks = bgroup "product" [bothMatched, leftMatched, rightMatched]
  where
    pair a b = (a, b)
    both :: Endpoint IO (Int, Text)
    both = productWith (pure 42) (pure "foo") pair
    left :: Endpoint IO (Int, Text)
    left = productWith (pure 42) empty pair
    right :: Endpoint IO (Int, Text)
    right = productWith empty (pure "foo") pair
    bothMatched = bench "bothMatched" $ nfIO (result . runEndpoint both $ getRoot)
    leftMatched = bench "leftMatched" $ nfIO (result . runEndpoint left $ getRoot)
    rightMatched = bench "rightMatched" $ nfIO (result . runEndpoint right $ getRoot)

alternativeBenchmarks :: Benchmark
alternativeBenchmarks = bgroup "alternative" [bothMatched, leftMatched, rightMatched]
  where
    both :: Endpoint IO Int
    both = pure 42 <|> pure 42
    left :: Endpoint IO Int
    left = pure 42 <|> empty
    right :: Endpoint IO Int
    right = empty <|> pure 42
    bothMatched = bench "bothMatched" $ nfIO (result . runEndpoint both $ getRoot)
    leftMatched = bench "leftMatched" $ nfIO (result . runEndpoint left $ getRoot)
    rightMatched = bench "rightMatched" $ nfIO (result . runEndpoint right $ getRoot)

mapBenchmarks :: Benchmark
mapBenchmarks = bgroup "map" [mapTen, mapTenM, mapTenOutput]
  where
    ten :: Endpoint IO Int
    ten = pure 10
    mapTen = bench "mapTen" $ nfIO (result . runEndpoint (fmap (+ 20) ten) $ getRoot)
    mapTenM = bench "mapTenM" $ nfIO (result . runEndpoint (mapM' (return . (+ 20)) ten) $ getRoot)
    mapTenOutput = bench "mapTenOutput" $ nfIO (result . runEndpoint (mapOutput (ok . (+ 20)) ten) $ getRoot)
    mapTenOutputM =
      bench "mapTenOutputM" $ nfIO (result . runEndpoint (mapOutputM (return . ok . (+ 20)) ten) $ getRoot)

bootstrapBenchmarks :: Benchmark
bootstrapBenchmarks = bgroup "bootstrap" [appBenchmark]
  where
    endpoint :: Endpoint IO [Foo]
    endpoint = lift (replicateM 128 fooFromChar <$> randomIO)
    app = bootstrap @TextPlain endpoint & compile & toApp
    callback response = seq response $ return ResponseReceived
    appBenchmark = bench "appBenchmark" $ whnfIO (app defaultRequest callback)

getFooBarBaz :: Input
getFooBarBaz = inputFromRequest $ defaultRequest {pathInfo = ["foo", "bar", "baz"]}

getTenTwenty :: Input
getTenTwenty = inputFromRequest $ defaultRequest {pathInfo = ["10", "20"]}

getRoot :: Input
getRoot = inputFromRequest defaultRequest

postPayload :: IO Input
postPayload = do
  ioRef <- newIORef $ C8.replicate 1024 'x'
  return $
    inputFromRequest $
    defaultRequest {requestBodyLength = KnownLength 1024, requestBody = atomicModifyIORef ioRef (mempty, )}

result :: (Applicative m) => EndpointResult m a -> m (Maybe a)
result Matched {..} = fmap (foldr (const . Just) Nothing) matchedOutput
result _            = pure Nothing

instance Encode TextPlain SomeException where
  encode = mempty

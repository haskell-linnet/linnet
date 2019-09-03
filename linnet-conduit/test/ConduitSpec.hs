{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module ConduitSpec where

import           Conduit                   (ConduitT, runConduit, yield, (.|))
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Builder   as Builder
import           Data.ByteString.Char8     (unpack)
import qualified Data.ByteString.Lazy      as BL
import           Data.Conduit.Combinators  (sinkList)
import           Data.IORef                (atomicModifyIORef, modifyIORef,
                                            newIORef, readIORef)
import           Data.List                 (intersperse)
import           Linnet
import           Linnet.Conduit
import           Linnet.Endpoint           (EndpointResult (..), isMatched,
                                            runEndpoint)
import           Linnet.Input
import           Linnet.ToResponse
import           Network.HTTP.Types.Status (ok200)
import           Network.Wai               (RequestBodyLength (..),
                                            defaultRequest, requestBody,
                                            requestBodyLength, responseStream,
                                            responseToStream)
import           Test.Hspec
import           Test.QuickCheck           (property)
import           Test.QuickCheck.Instances
import           Test.QuickCheck.Monadic   (assert, monadicIO)

spec :: Spec
spec =
  describe "Conduit streaming" $ do
    describe "streamBody" $ do
      it "should match only Chunked requests" $ do
        let streamed = inputFromRequest $ defaultRequest {requestBodyLength = ChunkedBody}
        let fixed = inputFromRequest defaultRequest
        let endpoint = streamBody @IO
        isMatched (runEndpoint endpoint streamed) `shouldBe` True
        isMatched (runEndpoint endpoint fixed) `shouldBe` False
      it "streams request body as conduit stream" $
        property $ \(bytes :: [BS.ByteString]) ->
          let nonEmpty = filter (/= mempty) bytes
              getChunk :: [BS.ByteString] -> ([BS.ByteString], BS.ByteString)
              getChunk []    = ([], BS.empty)
              getChunk (h:t) = (t, h)
           in monadicIO $ do
                ref <- liftIO $ newIORef nonEmpty
                let streamed =
                      inputFromRequest $
                      defaultRequest {requestBodyLength = ChunkedBody, requestBody = atomicModifyIORef ref getChunk}
                let endpoint = streamBody @IO ~> (\stream -> ok <$> runConduit (stream .| sinkList))
                let (Matched _ mOut) = runEndpoint endpoint streamed
                output <- liftIO mOut
                assert $ output == ok (map BL.fromStrict nonEmpty)
    describe "ToResponse conduit" $ do
      it "should create chunked response" $
        property $ \(bytes :: [BS.ByteString]) ->
          let stream :: [BS.ByteString] -> ConduitT () BS.ByteString IO ()
              stream = foldr ((>>) . yield) (return ())
           in monadicIO $ do
                final <- liftIO $ newIORef ([] @BS.ByteString)
                buffer <- liftIO $ newIORef ([] @BS.ByteString)
                let response = toResponse @TextPlain ok200 [] $ stream bytes
                let (_, _, responseBody) = responseToStream response
                let write builder = modifyIORef buffer (++ [BL.toStrict $ Builder.toLazyByteString builder])
                let flush = do
                      buf <- readIORef buffer
                      _ <- modifyIORef final (++ buf)
                      _ <- modifyIORef buffer (const [])
                      return ()
                result <- liftIO $ responseBody (\fn -> fn write flush >> readIORef final)
                assert (result == bytes)
      it "should intersperse newline symbol in JSON stream" $
        property $ \(bytes :: [BS.ByteString]) ->
          let stream :: [BS.ByteString] -> ConduitT () BS.ByteString IO ()
              stream = foldr ((>>) . yield) (return ())
           in monadicIO $ do
                final <- liftIO $ newIORef ([] @BS.ByteString)
                buffer <- liftIO $ newIORef ([] @BS.ByteString)
                let response = toResponse @ApplicationJson ok200 [] $ stream bytes
                let (_, _, responseBody) = responseToStream response
                let write builder = modifyIORef buffer (++ [BL.toStrict $ Builder.toLazyByteString builder])
                let flush = do
                      buf <- readIORef buffer
                      _ <- modifyIORef final (++ buf)
                      _ <- modifyIORef buffer (const [])
                      return ()
                result <- liftIO $ responseBody (\fn -> fn write flush >> readIORef final)
                assert
                  (result ==
                   if null bytes
                     then []
                     else intersperse "\n" bytes ++ ["\n"])

instance Encode ApplicationJson BS.ByteString where
  encode = BL.fromStrict

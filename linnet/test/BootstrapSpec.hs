{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module BootstrapSpec where

import           Test.Hspec

import           Control.Concurrent        (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad.Catch       (throwM)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (ReaderT (..))
import qualified Data.CaseInsensitive      as CI
import           Data.Function             ((&))
import           Data.Functor.Identity     (runIdentity)
import qualified Data.Text                 as T
import           Instances
import           Linnet
import           Linnet.Bootstrap
import           Linnet.Endpoint
import           Linnet.Errors
import           Linnet.Internal.Coproduct ((:+:), CNil)
import           Linnet.Output
import           Linnet.ToResponse         (NotAcceptable406, toResponse)
import           Network.HTTP.Types        (hAccept, hContentType, methodPost,
                                            status400, status404, status405,
                                            status406)
import           Network.HTTP.Types.Header (hAllow)
import           Network.Wai               (defaultRequest, pathInfo,
                                            requestHeaders, responseHeaders,
                                            responseStatus)
import           Network.Wai.Internal      (ResponseReceived (..))
import           Test.QuickCheck           (property)
import           Test.QuickCheck.Monadic   (assert, monadicIO)

spec :: Spec
spec = do
  it "compiles endpoint to Kleisli" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let readerT = bootstrap @TextPlain (liftOutputM (return out)) & compile
        result <- liftIO $ runReaderT readerT defaultRequest
        assert (result == outputToResponse (toResponse @TextPlain) (toResponse @TextPlain) (toResponse @TextPlain) out)
  it "responds with corresponding content-type" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let readerT = bootstrap @TextPlain (liftOutputM (return out)) & compile
        result <- liftIO $ runReaderT readerT defaultRequest
        let maybeContentType = lookup (CI.mk "Content-Type") (responseHeaders result)
        assert (maybeContentType == Just "text/plain")
  it "responds with 404" $ do
    let readerT = bootstrap @TextPlain (get (p' "foo") ~>> (return . ok $ ("text" :: T.Text))) & compile
    result <- liftIO $ runReaderT readerT defaultRequest
    responseStatus result `shouldBe` status404
  it "responds with 400 on LinnetError" $ do
    let endpoint = liftOutputM (throwM $ DecodeError "oops") :: Endpoint IO T.Text
    let readerT = bootstrap @TextPlain endpoint & compile
    result <- runReaderT readerT defaultRequest
    responseStatus result `shouldBe` status400
  it "responds with 405 on method mismatch" $ do
    let readerT = bootstrap @TextPlain (post (p' "foo") ~>> (return . ok $ ("text" :: T.Text))) & compile
    result <- runReaderT readerT defaultRequest {pathInfo = ["foo"]}
    responseStatus result `shouldBe` status405
    responseHeaders result `shouldBe` [(hAllow, methodPost)]
  it "serves different content-types" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let text = get (p' "foo") ~>> return out
        let html = get (p' "bar") ~>> return out
        let readerT = bootstrap @TextPlain text & serve @TextHtml html & compile
        textResult <- liftIO $ runReaderT readerT (defaultRequest {pathInfo = ["foo"]})
        htmlResult <- liftIO $ runReaderT readerT (defaultRequest {pathInfo = ["bar"]})
        let maybeTextContentType = lookup hContentType (responseHeaders textResult)
        let maybeHtmlContentType = lookup hContentType (responseHeaders htmlResult)
        assert (maybeTextContentType == Just "text/plain")
        assert (maybeHtmlContentType == Just "text/html")
  it "negotiates content-type" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let text = get (p' "foo") ~>> return out
        let readerT = bootstrap @(TextPlain :+: TextHtml :+: NotAcceptable406) text & compile
        textResult <-
          liftIO $
          runReaderT
            readerT
            (defaultRequest {pathInfo = ["foo"], requestHeaders = [(hAccept, "text/plain; q=1.0, text/html; q=0.9")]})
        htmlResult <-
          liftIO $
          runReaderT
            readerT
            (defaultRequest {pathInfo = ["foo"], requestHeaders = [(hAccept, "text/plain; q=0.9, text/html; q=1.0")]})
        let maybeTextContentType = lookup hContentType (responseHeaders textResult)
        let maybeHtmlContentType = lookup hContentType (responseHeaders htmlResult)
        assert (maybeTextContentType == Just "text/plain")
        assert (maybeHtmlContentType == Just "text/html")
  it "returns 406 on failed negotiation" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let text = get (p' "foo") ~>> return out
        let readerT = bootstrap @(TextPlain :+: TextHtml :+: NotAcceptable406) text & compile
        textResult <-
          liftIO $
          runReaderT readerT (defaultRequest {pathInfo = ["foo"], requestHeaders = [(hAccept, "application/json")]})
        assert (responseStatus textResult == status406)
  it "falls back to the latest option when 406 is disabled" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let text = get (p' "foo") ~>> return out
        let readerT = bootstrap @(TextPlain :+: TextHtml) text & compile
        htmlResult <-
          liftIO $
          runReaderT readerT (defaultRequest {pathInfo = ["foo"], requestHeaders = [(hAccept, "application/json")]})
        let maybeHtmlContentType = lookup hContentType (responseHeaders htmlResult)
        assert (maybeHtmlContentType == Just "text/html")
  it "compiles into WAI application" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let app = bootstrap @TextPlain (liftOutputM (return out)) & compile & toApp @IO
        mvar <- liftIO newEmptyMVar
        let callback req = ResponseReceived <$ putMVar mvar req
        _ <- liftIO $ app defaultRequest callback
        response <- liftIO $ takeMVar mvar
        assert
          (response == outputToResponse (toResponse @TextPlain) (toResponse @TextPlain) (toResponse @TextPlain) out)

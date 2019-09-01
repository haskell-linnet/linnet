{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module BootstrapSpec where

import           Test.Hspec

import           Control.Concurrent      (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad.Catch     (throwM)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (ReaderT (..))
import qualified Data.CaseInsensitive    as CI
import           Data.Function           ((&))
import           Data.Functor.Identity   (runIdentity)
import qualified Data.Text               as T
import           Instances
import           Linnet
import           Linnet.Bootstrap
import           Linnet.Endpoint
import           Linnet.Errors
import           Linnet.Output
import           Network.HTTP.Types      (status400, status404)
import           Network.Wai             (defaultRequest, pathInfo,
                                          responseHeaders, responseStatus)
import           Network.Wai.Internal    (ResponseReceived (..))
import           Test.QuickCheck         (property)
import           Test.QuickCheck.Monadic (assert, monadicIO)

spec :: Spec
spec = do
  it "compiles endpoint to Kleisli" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let readerT = bootstrap @TextPlain (liftOutputM (return out)) & compile
        result <- liftIO $ runReaderT readerT defaultRequest
        assert (result == outputToResponse @T.Text @TextPlain out)
  it "responds with corresponding content-type" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let readerT = bootstrap @TextPlain (liftOutputM (return out)) & compile
        result <- liftIO $ runReaderT readerT defaultRequest
        let maybeContentType = lookup (CI.mk "Content-Type") (responseHeaders result)
        assert (maybeContentType == Just "text/plain")
  it "responds with 404" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let readerT = bootstrap @TextPlain (p' "foo" ~>> (return . ok $ ("text" :: T.Text))) & compile
        result <- liftIO $ runReaderT readerT defaultRequest
        assert (responseStatus result == status404)
  it "responds with 400 on LinnetError" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let endpoint = liftOutputM (throwM $ DecodeError "oops") :: Endpoint IO T.Text
        let readerT = bootstrap @TextPlain endpoint & compile
        result <- liftIO $ runReaderT readerT defaultRequest
        assert (responseStatus result == status400)
  it "serves different content-types" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let text = get (p' "foo") ~>> return out
        let html = get (p' "bar") ~>> return out
        let readerT = bootstrap @TextPlain text & serve @TextHtml html & compile
        textResult <- liftIO $ runReaderT readerT (defaultRequest {pathInfo = ["foo"]})
        htmlResult <- liftIO $ runReaderT readerT (defaultRequest {pathInfo = ["bar"]})
        let contentType = lookup (CI.mk "Content-Type")
        let maybeTextContentType = contentType (responseHeaders textResult)
        let maybeHtmlContentType = contentType (responseHeaders htmlResult)
        assert (maybeTextContentType == Just "text/plain")
        assert (maybeHtmlContentType == Just "text/html")
  it "compiles into WAI application" $
    property $ \(out :: (Output T.Text)) ->
      monadicIO $ do
        let app = bootstrap @TextPlain (liftOutputM (return out)) & compile & toApp @IO
        mvar <- liftIO newEmptyMVar
        let callback req = ResponseReceived <$ putMVar mvar req
        _ <- liftIO $ app defaultRequest callback
        response <- liftIO $ takeMVar mvar
        assert (response == outputToResponse @T.Text @TextPlain out)

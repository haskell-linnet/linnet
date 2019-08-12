{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Instances
  ( compareEndpoints
  , TestException(..)
  ) where

import           Control.Applicative                  (empty)
import           Control.Exception                    (Exception,
                                                       SomeException (..),
                                                       toException)
import           Control.Monad.Catch                  (MonadThrow, throwM)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as C8
import qualified Data.ByteString.Lazy                 as BL
import qualified Data.CaseInsensitive                 as CI
import           Data.Function                        ((&))
import           Data.List.NonEmpty                   (NonEmpty(..), toList)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import           Linnet                               (Decode (..), TextPlain)
import           Linnet.Endpoint
import           Linnet.Endpoints.Entity
import           Linnet.Errors
import           Linnet.Input
import           Linnet.Output
import qualified Network.HTTP.Types                   as HTTP
import           Network.Wai                          (Request, defaultRequest,
                                                       pathInfo, queryString,
                                                       requestBody,
                                                       requestHeaders,
                                                       requestMethod,
                                                       strictRequestBody)
import           Test.QuickCheck                      (Arbitrary (..),
                                                       CoArbitrary (..), Gen,
                                                       NonEmptyList, choose,
                                                       elements, getNonEmpty,
                                                       listOf, oneof, sample',
                                                       suchThat, vectorOf)
import           Test.QuickCheck.Instances.ByteString
import           Test.QuickCheck.Instances.Text
import           Util

genNonEmptyTuple :: Gen (B.ByteString, B.ByteString)
genNonEmptyTuple = do
  t1 <- suchThat arbitrary (/= B.empty)
  t2 <- suchThat arbitrary (/= B.empty)
  return (t1, t2)

genHeader :: Gen (CI.CI B.ByteString, B.ByteString)
genHeader = do
  (t1, t2) <- genNonEmptyTuple
  return (CI.mk t1, t2)

genParam :: Gen (B.ByteString, Maybe B.ByteString)
genParam = do
  (t1, t2) <- genNonEmptyTuple
  t2' <- elements [Just t2, Nothing]
  return (t1, t2')

genCookie :: Gen (CI.CI B.ByteString, B.ByteString)
genCookie = do
  kvs <- listOf genNonEmptyTuple
  let values = map (\t -> fst t `B.append` "=" `B.append` snd t) kvs
  return (CI.mk ("Cookie" :: B.ByteString), C8.intercalate ";" values)

genPath :: Gen [T.Text]
genPath = do
  size <- choose (0, 20)
  vectorOf size $ oneof [suchThat arbitrary (/= T.empty), T.pack . show <$> arbitrary @Int, elements ["true", "false"]]

genBody :: Gen B.ByteString
genBody = arbitrary

genMethod :: Gen B.ByteString
genMethod =
  elements
    [ HTTP.methodGet
    , HTTP.methodPost
    , HTTP.methodHead
    , HTTP.methodPut
    , HTTP.methodDelete
    , HTTP.methodTrace
    , HTTP.methodConnect
    , HTTP.methodOptions
    , HTTP.methodPatch
    ]

genRequest :: Gen Request
genRequest = do
  path <- genPath
  cookie <- genCookie
  headers <- listOf genHeader
  params <- listOf genParam
  body <- genBody
  method <- genMethod
  return
    defaultRequest
      { requestMethod = method
      , queryString = params
      , requestBody = pure body
      , pathInfo = path
      , requestHeaders = cookie : headers
      }

genInput :: Gen Input
genInput = do
  req <- genRequest
  return Input {reminder = pathInfo req, request = req}

genStatus :: Gen HTTP.Status
genStatus =
  elements
    [ HTTP.continue100
    , HTTP.switchingProtocols101
    , HTTP.ok200
    , HTTP.created201
    , HTTP.accepted202
    , HTTP.nonAuthoritative203
    , HTTP.noContent204
    , HTTP.resetContent205
    , HTTP.partialContent206
    , HTTP.multipleChoices300
    , HTTP.movedPermanently301
    , HTTP.found302
    , HTTP.seeOther303
    , HTTP.notModified304
    , HTTP.useProxy305
    , HTTP.temporaryRedirect307
    , HTTP.permanentRedirect308
    , HTTP.badRequest400
    , HTTP.unauthorized401
    , HTTP.paymentRequired402
    , HTTP.forbidden403
    , HTTP.notFound404
    , HTTP.methodNotAllowed405
    , HTTP.notAcceptable406
    , HTTP.proxyAuthenticationRequired407
    , HTTP.requestTimeout408
    , HTTP.conflict409
    , HTTP.gone410
    , HTTP.lengthRequired411
    , HTTP.preconditionFailed412
    , HTTP.requestEntityTooLarge413
    , HTTP.requestURITooLong414
    , HTTP.unsupportedMediaType415
    , HTTP.requestedRangeNotSatisfiable416
    , HTTP.expectationFailed417
    , HTTP.imATeapot418
    , HTTP.unprocessableEntity422
    , HTTP.preconditionRequired428
    , HTTP.tooManyRequests429
    , HTTP.requestHeaderFieldsTooLarge431
    , HTTP.internalServerError500
    , HTTP.notImplemented501
    , HTTP.badGateway502
    , HTTP.serviceUnavailable503
    , HTTP.gatewayTimeout504
    , HTTP.networkAuthenticationRequired511
    , HTTP.httpVersionNotSupported505
    ]

genOutputMeta :: Gen (HTTP.Status, [(CI.CI B.ByteString, B.ByteString)])
genOutputMeta = do
  status <- genStatus
  headers <- listOf genHeader
  cookie <- genCookie
  return (status, cookie : headers)

genEmptyOutput :: Gen (Output a)
genEmptyOutput = do
  (status, headers) <- genOutputMeta
  return $ Output {outputStatus = status, outputHeaders = headers, outputPayload = NoPayload}

genFailureOutput :: Gen (Output a)
genFailureOutput = do
  (status, headers) <- genOutputMeta
  str <- arbitrary
  return $ Output {outputStatus = status, outputHeaders = headers, outputPayload = ErrorPayload $ TestException str}

genPayloadOutput :: (Arbitrary a) => Gen (Output a)
genPayloadOutput = do
  (status, headers) <- genOutputMeta
  a <- arbitrary
  return $ Output {outputStatus = status, outputHeaders = headers, outputPayload = Payload a}

genOutput :: (Arbitrary a) => Gen (Output a)
genOutput = oneof [genPayloadOutput, genEmptyOutput, genFailureOutput]

genEmptyEndpoint :: (Monad m) => Gen (Endpoint m a)
genEmptyEndpoint = pure empty

genConstEndpoint :: (Monad m, Arbitrary a) => Gen (Endpoint m a)
genConstEndpoint = pure <$> arbitrary

genErrorEndpoint :: (MonadThrow m) => Gen (Endpoint m a)
genErrorEndpoint = lift . throwM . TestException <$> arbitrary

genEndpoint ::
     forall m a. (Arbitrary (Input -> a), Monad m)
  => Gen (Endpoint m a)
genEndpoint = do
  f <- arbitrary @(Input -> a)
  return $
    Endpoint
      { runEndpoint = \input -> Matched {matchedReminder = input, matchedOutput = pure $ ok (f input)}
      , toString = "arbitrary"
      }

genEntity :: Gen Entity
genEntity = oneof [genParamEntity, genHeaderEntity, genCookieEntity, genBodyEntity]
  where
    genParamEntity = Param <$> arbitrary
    genHeaderEntity = Header <$> arbitrary
    genCookieEntity = Cookie <$> arbitrary
    genBodyEntity = pure Body

genLinnetError :: Gen LinnetError
genLinnetError = oneof [genDecodeError, genMissingEntity, genEntityNotParsed]
  where
    genDecodeError = DecodeError <$> arbitrary
    genMissingEntity = MissingEntity <$> genEntity
    genEntityNotParsed = EntityNotParsed <$> genEntity <*> genLinnetError
    genLinnetErrors = LinnetErrors <$>
      do
        h <- genLinnetError
        t <- listOf genLinnetError
        return $ h :| t 
    

newtype TestException =
  TestException String
  deriving (Show, Eq)

instance Exception TestException

instance Arbitrary Request where
  arbitrary = genRequest

instance Arbitrary Input where
  arbitrary = genInput

instance CoArbitrary (CI.CI B.ByteString) where
  coarbitrary ci = coarbitrary (CI.original ci)

instance CoArbitrary Input where
  coarbitrary input gen =
    coarbitrary (reminder input) gen & coarbitrary ((requestMethod . request) input) &
    coarbitrary ((requestHeaders . request) input) &
    coarbitrary ((queryString . request) input)

instance Arbitrary LinnetError where
  arbitrary = genLinnetError
  

instance Arbitrary a => Arbitrary (Output a) where
  arbitrary = genOutput

instance (Arbitrary a, MonadThrow m) => Arbitrary (Endpoint m a) where
  arbitrary = oneof [genEndpoint, genErrorEndpoint, genConstEndpoint, genEmptyEndpoint]

instance (Eq a) => Eq (Payload a) where
  (==) (Payload a) (Payload b) = a == b
  (==) NoPayload NoPayload = True
  (==) (ErrorPayload ea) (ErrorPayload eb) = show (toException ea) == show (toException eb)
  (==) _ _ = False

instance (Eq a) => Eq (Output a) where
  (==) (Output sa pa ha) (Output sb pb hb) = sa == sb && pa == pb && ha == hb

instance Eq Input where
  (==) i i' = reminder i == reminder i' && (show . request) i == (show . request) i'

instance Eq SomeException where
  (==) e e' = show e == show e'

compareRequests :: Request -> Request -> IO Bool
compareRequests r r' = do
  b <- strictRequestBody r
  b' <- strictRequestBody r'
  return $ (show r' == show r) && b == b'

compareInputs :: Input -> Input -> IO Bool
compareInputs i i' = do
  r <- compareRequests (request i) (request i')
  return $ (reminder i == reminder i') && r

compareEndpointResults :: Eq (m (Output a)) => EndpointResult m a -> EndpointResult m a -> IO Bool
compareEndpointResults (Matched i m) (Matched i' m') = do
  eqInputs <- compareInputs i i'
  return $ (m == m) && eqInputs
compareEndpointResults NotMatched NotMatched = pure True
compareEndpointResults _ _ = pure False

compareEndpoints :: (Eq (m (Output a))) => Endpoint m a -> Endpoint m a -> IO Bool
compareEndpoints ea eb = do
  input <- head <$> sample' genInput
  compareEndpointResults (runEndpoint ea input) (runEndpoint eb input)

instance Decode TextPlain T.Text where
  decode = Right . TE.decodeUtf8 . BL.toStrict

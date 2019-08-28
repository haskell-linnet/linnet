{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Instances
  ( TestException(..)
  , Foo(..)
  ) where

import           Control.Applicative                  (empty)
import           Control.Exception                    (Exception,
                                                       SomeException (..),
                                                       catch, toException)
import qualified Control.Monad.Catch                  as MC
import qualified Data.ByteString                      as B
import           Data.ByteString.Builder              (toLazyByteString)
import qualified Data.ByteString.Builder              as Builder
import qualified Data.ByteString.Char8                as C8
import           Data.ByteString.Conversion           (ToByteString (..))
import qualified Data.ByteString.Lazy                 as BL
import qualified Data.ByteString.Lazy.Char8           as C8L
import qualified Data.CaseInsensitive                 as CI
import           Data.Function                        ((&))
import           Data.List.NonEmpty                   (NonEmpty (..), toList)
import           Data.Maybe                           (listToMaybe)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import           GHC.IO                               (unsafePerformIO)
import           Linnet                               (Decode (..), Encode (..),
                                                       TextHtml, TextPlain)
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
import           Network.Wai.Internal                 (Response (..))
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
  method <- genMethod
  return
    defaultRequest
      { requestMethod = method
      , queryString = params
      , requestBody = pure mempty
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

genEmptyEndpoint :: (MC.MonadCatch m) => Gen (Endpoint m a)
genEmptyEndpoint = pure empty

genConstEndpoint :: (MC.MonadCatch m, Arbitrary a) => Gen (Endpoint m a)
genConstEndpoint = pure <$> arbitrary

genErrorEndpoint :: (MC.MonadThrow m) => Gen (Endpoint m a)
genErrorEndpoint = lift . MC.throwM . TestException <$> arbitrary

genEndpoint ::
     forall m a. (Arbitrary (Input -> Output a), Monad m)
  => Gen (Endpoint m a)
genEndpoint = do
  f <- arbitrary @(Input -> Output a)
  return $
    Endpoint
      { runEndpoint = \input -> Matched {matchedReminder = input, matchedOutput = pure $ (f input)}
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
    genLinnetErrors =
      LinnetErrors <$> do
        h <- genLinnetError
        t <- listOf genLinnetError
        return $ h :| t

newtype TestException =
  TestException String
  deriving (Show, Eq)

instance Exception TestException

instance Arbitrary Request where
  arbitrary = genRequest

instance Arbitrary Entity where
  arbitrary = genEntity

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

instance (Arbitrary a, MC.MonadCatch m) => Arbitrary (Endpoint m a) where
  arbitrary = oneof [genEndpoint, genErrorEndpoint, genConstEndpoint, genEmptyEndpoint]

instance Eq Input where
  (==) i i' = reminder i == reminder i' && request i == request i'

instance Eq SomeException where
  (==) e e' = show e == show e'

instance Eq Request where
  (==) r r' = show r == show r'

instance Eq (m (Output a)) => Eq (EndpointResult m a) where
  (==) (Matched i m) (Matched i' m') = i == i' && m == m'
  (==) NotMatched NotMatched         = True
  (==) _ _                           = False

instance Eq (m (Output a)) => Eq (Endpoint m a) where
  (==) ea eb = runEndpoint ea input == runEndpoint eb input
    where
      input = head . unsafePerformIO . sample' $ genInput

instance Decode TextPlain T.Text where
  decode = Right . TE.decodeUtf8 . BL.toStrict

instance Eq a => Eq (IO a) where
  (==) i i' = unsafePerformIO (tryAll i) == unsafePerformIO (tryAll i')
    where
      tryAll m = MC.catchAll (fmap Right m) (pure . Left)

instance Eq Response where
  (==) (ResponseBuilder s hs b) (ResponseBuilder s' hs' b') =
    s == s && hs == hs' && toLazyByteString b == toLazyByteString b'

newtype Foo =
  Foo
    { x :: String
    }
  deriving (Show, Read, Eq)

instance Arbitrary Foo where
  arbitrary = Foo <$> arbitrary

instance Decode TextPlain Foo where
  decode bs =
    case (reads . C8.unpack . BL.toStrict) bs of
      [(foo, _)] -> Right foo
      _          -> Left (DecodeError "Couldn't parse Foo")

instance Encode ct SomeException where
  encode = C8L.pack . show

instance Encode TextHtml T.Text where
  encode = BL.fromStrict . TE.encodeUtf8

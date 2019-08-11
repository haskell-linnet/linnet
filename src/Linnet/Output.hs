{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}

module Linnet.Output
  ( Output(..)
  , Payload(..)
  , ok
  , created
  , accepted
  , noContent
  , badRequest
  , unauthorized
  , paymentRequired
  , forbidden
  , notFound
  , methodNotAllowed
  , notAcceptable
  , conflict
  , gone
  , lengthRequired
  , preconditionFailed
  , requestEntityTooLarge
  , unprocessableEntity
  , tooManyRequests
  , internalServerError
  , notImplemented
  , badGateway
  , serviceUnavailable
  , gatewayTimeout
  , payloadOutput
  , payloadError
  , payloadEmpty
  , transformM
  , withHeader
  , outputToResponse
  ) where

import           Control.Exception         (Exception, SomeException,
                                            toException)
import qualified Data.ByteString           as B
import qualified Data.CaseInsensitive      as CI
import           Linnet.ToResponse         (ToResponse (..))
import           Network.HTTP.Types        (Header)
import           Network.HTTP.Types.Status
import           Network.Wai

data Payload a
  = Payload a
  | NoPayload
  | forall e. Exception e =>
              ErrorPayload e

deriving instance (Show a) => Show (Payload a)

data Output a =
  Output
    { outputStatus  :: Status
    , outputPayload :: Payload a
    , outputHeaders :: [Header]
    }

instance (Show a) => Show (Output a) where
  show out = "Output(" ++ show (statusCode $ outputStatus out) ++ ", " ++ show (outputPayload out) ++ ")"

instance Functor Output where
  fmap f (Output status (Payload a) headers) = Output status (Payload (f a)) headers
  fmap _ (Output status NoPayload headers) = Output status NoPayload headers
  fmap _ (Output status (ErrorPayload e) headers) = Output status (ErrorPayload e) headers

instance Applicative Output where
  pure = ok
  (<*>) (Output _ (Payload f) _) (Output status (Payload a) headers) =
    Output {outputStatus = status, outputPayload = Payload (f a), outputHeaders = headers}
  (<*>) _ (Output status NoPayload headers) =
    Output {outputStatus = status, outputPayload = NoPayload, outputHeaders = headers}
  (<*>) _ (Output status (ErrorPayload e) headers) =
    Output {outputStatus = status, outputPayload = ErrorPayload e, outputHeaders = headers}

instance Monad Output where
  (>>=) (Output _ (Payload a) _) f = f a
  (>>=) (Output status NoPayload headers) _ =
    Output {outputStatus = status, outputPayload = NoPayload, outputHeaders = headers}
  (>>=) (Output status (ErrorPayload e) headers) _ =
    Output {outputStatus = status, outputPayload = ErrorPayload e, outputHeaders = headers}

instance Foldable Output where
  foldMap fn (Output _ (Payload a) _) = fn a
  foldMap _ _                         = mempty

instance Traversable Output where
  traverse fn (Output status (Payload a) headers) = (\v -> Output status (Payload v) headers) <$> fn a
  traverse _ (Output status NoPayload headers) =
    pure $ Output {outputStatus = status, outputPayload = NoPayload, outputHeaders = headers}
  traverse _ (Output status (ErrorPayload e) headers) =
    pure $ Output {outputStatus = status, outputPayload = ErrorPayload e, outputHeaders = headers}

withHeader :: (B.ByteString, B.ByteString) -> Output a -> Output a
withHeader (k, v) (Output status payload headers) =
  Output {outputStatus = status, outputPayload = payload, outputHeaders = (CI.mk k, v) : headers}

transformM :: (Applicative m) => (a -> m (Output b)) -> Output a -> m (Output b)
transformM fn (Output _ (Payload a) _) = fn a
transformM _ (Output status NoPayload headers) =
  pure $ Output {outputStatus = status, outputPayload = NoPayload, outputHeaders = headers}
transformM _ (Output status (ErrorPayload e) headers) =
  pure $ Output {outputStatus = status, outputPayload = ErrorPayload e, outputHeaders = headers}

ok :: a -> Output a
ok = payloadOutput ok200

created :: a -> Output a
created = payloadOutput created201

accepted :: Output a
accepted = payloadEmpty accepted202

noContent :: Output a
noContent = payloadEmpty noContent204

badRequest :: (Exception e) => e -> Output a
badRequest = payloadError badRequest400

unauthorized :: (Exception e) => e -> Output a
unauthorized = payloadError unauthorized401

paymentRequired :: (Exception e) => e -> Output a
paymentRequired = payloadError paymentRequired402

forbidden :: (Exception e) => e -> Output a
forbidden = payloadError forbidden403

notFound :: (Exception e) => e -> Output a
notFound = payloadError notFound404

methodNotAllowed :: (Exception e) => e -> Output a
methodNotAllowed = payloadError methodNotAllowed405

notAcceptable :: (Exception e) => e -> Output a
notAcceptable = payloadError notAcceptable406

conflict :: (Exception e) => e -> Output a
conflict = payloadError conflict409

gone :: (Exception e) => e -> Output a
gone = payloadError gone410

lengthRequired :: (Exception e) => e -> Output a
lengthRequired = payloadError lengthRequired411

preconditionFailed :: (Exception e) => e -> Output a
preconditionFailed = payloadError preconditionFailed412

requestEntityTooLarge :: (Exception e) => e -> Output a
requestEntityTooLarge = payloadError requestEntityTooLarge413

unprocessableEntity :: (Exception e) => e -> Output a
unprocessableEntity = payloadError unprocessableEntity422

tooManyRequests :: (Exception e) => e -> Output a
tooManyRequests = payloadError tooManyRequests429

internalServerError :: (Exception e) => e -> Output a
internalServerError = payloadError internalServerError500

notImplemented :: (Exception e) => e -> Output a
notImplemented = payloadError notImplemented501

badGateway :: (Exception e) => e -> Output a
badGateway = payloadError badGateway502

serviceUnavailable :: (Exception e) => e -> Output a
serviceUnavailable = payloadError serviceUnavailable503

gatewayTimeout :: (Exception e) => e -> Output a
gatewayTimeout = payloadError gatewayTimeout504

payloadOutput :: Status -> a -> Output a
payloadOutput status payload = Output {outputStatus = status, outputPayload = Payload payload, outputHeaders = []}

payloadError :: (Exception e) => Status -> e -> Output a
payloadError status err = Output {outputStatus = status, outputPayload = ErrorPayload err, outputHeaders = []}

payloadEmpty :: Status -> Output a
payloadEmpty status = Output {outputStatus = status, outputPayload = NoPayload, outputHeaders = []}

outputToResponse ::
     forall a ct. (ToResponse ct a, ToResponse ct SomeException)
  => Output a
  -> Response
outputToResponse output =
  let response =
        case outputPayload output of
          Payload a      -> toResponse @ct a
          NoPayload      -> responseLBS (outputStatus output) [] mempty
          ErrorPayload e -> toResponse @ct $ toException e
   in mapResponseStatus (const (outputStatus output)) response

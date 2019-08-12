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

-- | Output of 'Endpoint' that carries some 'Payload' @a@ together with response status and headers
data Output a =
  Output
    { outputStatus  :: Status
    , outputPayload :: Payload a
    , outputHeaders :: [Header]
    }

-- | Payload of 'Output' that could be:
data Payload a
  = Payload a -- ^ Payload with some value @a@
  | NoPayload -- ^ Represents empty response
  | forall e. Exception e =>
              ErrorPayload e -- ^ Failed payload with an exception inside

deriving instance (Show a) => Show (Payload a)

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

-- | Add header to given 'Output'
withHeader :: (B.ByteString, B.ByteString) -> Output a -> Output a
withHeader (k, v) (Output status payload headers) =
  Output {outputStatus = status, outputPayload = payload, outputHeaders = (CI.mk k, v) : headers}

-- | Transform payload of output
transformM :: (Applicative m) => (a -> m (Output b)) -> Output a -> m (Output b)
transformM fn (Output _ (Payload a) _) = fn a
transformM _ (Output status NoPayload headers) =
  pure $ Output {outputStatus = status, outputPayload = NoPayload, outputHeaders = headers}
transformM _ (Output status (ErrorPayload e) headers) =
  pure $ Output {outputStatus = status, outputPayload = ErrorPayload e, outputHeaders = headers}

-- | Create 'Output' with 'Payload' @a@ and status @OK 200@
ok :: a -> Output a
ok = payloadOutput ok200

-- | Create 'Output' with 'Payload' @a@ and status @CREATED 201@
created :: a -> Output a
created = payloadOutput created201

-- | Create 'Output' with @NoPayload@ and status @ACCEPTED 202@
accepted :: Output a
accepted = payloadEmpty accepted202

-- | Create 'Output' with @NoPayload@ and status @NO CONTENT 202@
noContent :: Output a
noContent = payloadEmpty noContent204

-- | Create 'Output' with @ErrorPayload e@ and status @BAD REQUEST 400@
badRequest :: (Exception e) => e -> Output a
badRequest = payloadError badRequest400

-- | Create 'Output' with @ErrorPayload e@ and status @UNAUTHORIZED 401@
unauthorized :: (Exception e) => e -> Output a
unauthorized = payloadError unauthorized401

-- | Create 'Output' with @ErrorPayload e@ and status @PAYMENT REQUIRED 402@
paymentRequired :: (Exception e) => e -> Output a
paymentRequired = payloadError paymentRequired402

-- | Create 'Output' with @ErrorPayload e@ and status @FORBIDDEN 403@
forbidden :: (Exception e) => e -> Output a
forbidden = payloadError forbidden403

-- | Create 'Output' with @ErrorPayload e@ and status @NOT FOUND 404@
notFound :: (Exception e) => e -> Output a
notFound = payloadError notFound404

-- | Create 'Output' with @ErrorPayload e@ and status @METHOD NOT ALLOWED 405@
methodNotAllowed :: (Exception e) => e -> Output a
methodNotAllowed = payloadError methodNotAllowed405

-- | Create 'Output' with @ErrorPayload e@ and status @NOT ACCEPTABLE 406@
notAcceptable :: (Exception e) => e -> Output a
notAcceptable = payloadError notAcceptable406

-- | Create 'Output' with @ErrorPayload e@ and status @CONFLICT 409@
conflict :: (Exception e) => e -> Output a
conflict = payloadError conflict409

-- | Create 'Output' with @ErrorPayload e@ and status @GONE 410@
gone :: (Exception e) => e -> Output a
gone = payloadError gone410

-- | Create 'Output' with @ErrorPayload e@ and status @LENGTH REQUIRED 411@
lengthRequired :: (Exception e) => e -> Output a
lengthRequired = payloadError lengthRequired411

-- | Create 'Output' with @ErrorPayload e@ and status @PRECONDITIONED FAILED 412@
preconditionFailed :: (Exception e) => e -> Output a
preconditionFailed = payloadError preconditionFailed412

-- | Create 'Output' with @ErrorPayload e@ and status @REQUEST ENTITY TOO LARGE 413@
requestEntityTooLarge :: (Exception e) => e -> Output a
requestEntityTooLarge = payloadError requestEntityTooLarge413

-- | Create 'Output' with @ErrorPayload e@ and status @UNPROCESSABLE ENTITY 422@
unprocessableEntity :: (Exception e) => e -> Output a
unprocessableEntity = payloadError unprocessableEntity422

-- | Create 'Output' with @ErrorPayload e@ and status @TOO MANY REQUESTS 422@
tooManyRequests :: (Exception e) => e -> Output a
tooManyRequests = payloadError tooManyRequests429

-- | Create 'Output' with @ErrorPayload e@ and status @INTERNAL SERVER ERROR 500@
internalServerError :: (Exception e) => e -> Output a
internalServerError = payloadError internalServerError500

-- | Create 'Output' with @ErrorPayload e@ and status @NOT IMPLEMENTED 501@
notImplemented :: (Exception e) => e -> Output a
notImplemented = payloadError notImplemented501

-- | Create 'Output' with @ErrorPayload e@ and status @BAD GATEWAY 502@
badGateway :: (Exception e) => e -> Output a
badGateway = payloadError badGateway502

-- | Create 'Output' with @ErrorPayload e@ and status @SERVICE UNAVAILABLE 503@
serviceUnavailable :: (Exception e) => e -> Output a
serviceUnavailable = payloadError serviceUnavailable503

-- | Create 'Output' with @ErrorPayload e@ and status @GATEWAY TIMEOUT 504@
gatewayTimeout :: (Exception e) => e -> Output a
gatewayTimeout = payloadError gatewayTimeout504

-- | Create successful 'Output' with payload @a@ and given status
payloadOutput :: Status -> a -> Output a
payloadOutput status payload = Output {outputStatus = status, outputPayload = Payload payload, outputHeaders = []}

-- | Create failed 'Output' with exception @e@ and given status
payloadError :: (Exception e) => Status -> e -> Output a
payloadError status err = Output {outputStatus = status, outputPayload = ErrorPayload err, outputHeaders = []}

-- | Create empty 'Output' with given status
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

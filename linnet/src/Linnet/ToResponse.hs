{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Linnet.ToResponse
  ( ToResponse(..)
  , Negotiable(..)
  , NotAcceptable406
  ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Linnet.Encode (Encode(..))
import Linnet.Internal.Coproduct ((:+:), CNil, Coproduct(..))
import Network.HTTP.Media (MediaType, Quality, (//), matchQuality)
import Network.HTTP.Types (Header, Status, hContentType, notAcceptable406, status404)
import Network.Wai (Response, responseLBS)

-- | Type-class to convert a value of type @a@ into Response with Content-Type of @ct@
class ToResponse ct a where
  toResponse :: Status -> [Header] -> a -> Response

instance {-# OVERLAPPABLE #-} (ToResponse' (ValueT a) ct a) => ToResponse ct a where
  toResponse = toResponse' @(ValueT a) @ct

class ToResponse' (value :: Value) ct a where
  toResponse' :: Status -> [Header] -> a -> Response

instance (Encode (Proxy ct) a, KnownSymbol ct) => ToResponse' 'Value (Proxy ct) a where
  toResponse' status headers a = mkResponse @ct status headers $ encode @(Proxy ct) a

instance ToResponse' 'ResponseValue ct Response where
  toResponse' _ _ = id

instance (KnownSymbol ct) => ToResponse' 'UnitValue (Proxy ct) () where
  toResponse' status headers _ = mkResponse @ct status headers mempty

instance ToResponse' 'CNilValue ct CNil where
  toResponse' _ _ _ = responseLBS status404 [] mempty

instance (ToResponse ct a, ToResponse ct b) => ToResponse' 'CoproductValue ct (Coproduct a b) where
  toResponse' status headers (Inl a) = toResponse @ct status headers a
  toResponse' status headers (Inr b) = toResponse @ct status headers b

mkResponse ::
     forall ct. (KnownSymbol ct)
  => Status
  -> [Header]
  -> BL.ByteString
  -> Response
mkResponse status headers = responseLBS status ((hContentType, C8.pack $ symbolVal (Proxy :: Proxy ct)) : headers)

data Value
  = Value
  | ResponseValue
  | CoproductValue
  | CNilValue
  | UnitValue

type family ValueT (a :: *) :: Value where
  ValueT (Coproduct _ _) = 'CoproductValue
  ValueT CNil = 'CNilValue
  ValueT Response = 'ResponseValue
  ValueT () = 'UnitValue
  ValueT _ = 'Value

type ToResponseF a = Status -> [Header] -> a -> Response

-- | Type-class that enables Content-Type negotiation between client and server baked by instances of 'ToResponse'.
class Negotiable cts a where
  negotiate :: [Quality MediaType] -> Maybe (MediaType, ToResponseF a) -> ToResponseF a

instance (Negotiable' (ContentTypeValueT cts) cts a) => Negotiable cts a where
  negotiate = negotiate' @(ContentTypeValueT cts) @cts

class Negotiable' (t :: ContentTypeValue) cts a where
  negotiate' :: [Quality MediaType] -> Maybe (MediaType, ToResponseF a) -> ToResponseF a

instance (KnownSymbol c, ToResponse (Proxy c) a, Negotiable t a) =>
         Negotiable' 'ContentTypeCoproduct (Proxy c :+: t) a where
  negotiate' accept bestMatch = acceptMatcher accept
    where
      acceptMatcher mediaType =
        let value = C8.pack $ symbolVal (Proxy :: Proxy c)
            [p, s] = C8.split '/' value
            mt = p // s
            bestMatchExists = do
              (bestMatchMediaType, _) <- bestMatch
              match <- matchQuality [bestMatchMediaType, mt] mediaType
              if match == bestMatchMediaType
                then pure $ negotiate @t mediaType bestMatch
                else pure $ negotiate @t mediaType (Just (match, toResponse @(Proxy c)))
            bestMatchUnknown = do
              match <- matchQuality [mt] mediaType
              pure $ negotiate @t mediaType (Just (match, toResponse @(Proxy c)))
            noMatchExists = negotiate @t mediaType Nothing
         in fromMaybe noMatchExists (bestMatchExists <|> bestMatchUnknown)

instance Negotiable' 'ContentTypeNegotiationFailed NotAcceptable406 a where
  negotiate' _ (Just (_, fn)) = fn
  negotiate' _ Nothing = \_ _ _ -> responseLBS notAcceptable406 [] mempty

instance ToResponse cts a => Negotiable' 'ContentTypeValue cts a where
  negotiate' _ _ = toResponse @cts

data ContentTypeValue
  = ContentTypeValue
  | ContentTypeNegotiationFailed
  | ContentTypeCoproduct

type family ContentTypeValueT ct :: ContentTypeValue where
  ContentTypeValueT (Coproduct _ _) = 'ContentTypeCoproduct
  ContentTypeValueT NotAcceptable406 = 'ContentTypeNegotiationFailed
  ContentTypeValueT _ = 'ContentTypeValue

-- | Uninhabited type to signal the need of 406 error during Content-Type negotiation
data NotAcceptable406
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Linnet.ToResponse
  ( ToResponse(..)
  ) where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Proxy (Proxy(..))
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Linnet.Encode (Encode(..))
import Linnet.Internal.Coproduct (CNil, Coproduct(..))
import Network.HTTP.Types (status200, status404)
import Network.Wai (Response, responseLBS)

class ToResponse (ct :: Symbol) a where
  toResponse :: a -> Response

instance (ToResponse' (ValueT a) ct a) => ToResponse ct a where
  toResponse = toResponse' @(ValueT a) @ct

class ToResponse' (value :: Value) (ct :: Symbol) a where
  toResponse' :: a -> Response

instance (Encode ct a, KnownSymbol ct) => ToResponse' 'Value ct a where
  toResponse' a = mkResponse @ct $ encode @ct a

instance ToResponse' 'ResponseValue ct Response where
  toResponse' = id

instance (KnownSymbol ct) => ToResponse' 'UnitValue ct () where
  toResponse' _ = mkResponse @ct mempty

instance ToResponse' 'CNilValue ct CNil where
  toResponse' _ = responseLBS status404 [] mempty

instance (ToResponse ct a, ToResponse ct b) => ToResponse' 'CoproductValue ct (Coproduct a b) where
  toResponse' (Inl a) = toResponse @ct a
  toResponse' (Inr b) = toResponse @ct b

mkResponse ::
     forall ct. (KnownSymbol ct)
  => BL.ByteString
  -> Response
mkResponse = responseLBS status200 [("Content-Type", C8.pack $ symbolVal (Proxy :: Proxy ct))]

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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module EncodeLaws
  ( encodeLaws
  , encodeTextLaws
  ) where

import           Data.ByteString.Conversion (ToByteString, toByteString)
import           Data.Data                  (Proxy)
import           GHC.Base                   (Symbol)
import           Linnet                     (TextPlain)
import           Linnet.Encode
import           Test.QuickCheck            (Arbitrary, property)
import           Test.QuickCheck.Classes    (Laws (..))

encodeLaws ::
     forall a ct . (ToByteString a, Encode ct a, Arbitrary a, Show a)
  => Laws
encodeLaws = Laws "Encode" properties
  where
    roundTrip = property $ \(a :: a) -> encode @ct @a a == toByteString a
    properties = [("roundTrip", roundTrip)]

encodeTextLaws ::
     forall a. (ToByteString a, Encode TextPlain a, Arbitrary a, Show a)
  => Laws
encodeTextLaws = encodeLaws @a @TextPlain

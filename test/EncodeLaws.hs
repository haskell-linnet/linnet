{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module EncodeLaws
  ( encodeLaws
  , encodeTextLaws
  ) where

import           Data.ByteString.Conversion (ToByteString, toByteString)
import           GHC.Base                   (Symbol)
import           Linnet                     (TextPlain)
import           Linnet.Encode
import           Test.QuickCheck            (Arbitrary, property)
import           Test.QuickCheck.Classes    (Laws (..))

encodeLaws ::
     forall a (ct :: Symbol). (ToByteString a, Encode ct a, Arbitrary a, Show a)
  => Laws
encodeLaws = Laws "Encode" properties
  where
    roundTrip = property $ \(a :: a) -> encode @ct @a a == toByteString a
    properties = [("roundTrip", roundTrip)]

encodeTextLaws ::
     forall a. (ToByteString a, Encode TextPlain a, Arbitrary a, Show a)
  => Laws
encodeTextLaws = encodeLaws @a @TextPlain

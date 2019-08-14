{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DecodeLaws where

import           Data.ByteString.Conversion (ToByteString, toByteString')
import           Data.Text                  (pack)
import qualified Data.Text                  as T
import           Linnet.Decode
import           Linnet.Endpoints.Entity    (Entity)
import           Test.QuickCheck            (Arbitrary, property)
import           Test.QuickCheck.Classes    (Laws (..))

decodePathLaws ::
     forall a. (DecodePath a, Arbitrary a, Show a, Eq a)
  => (a -> T.Text)
  -> Laws
decodePathLaws encode = Laws "DecodePath" laws
  where
    roundTrip = property $ \(a :: a) -> decodePath (encode a) == Just a
    laws = [("roundTrip", roundTrip)]

decodeEntityLaws ::
     forall a. (DecodeEntity a, Arbitrary a, Show a, Eq a, Arbitrary Entity, ToByteString a)
  => Laws
decodeEntityLaws = Laws "DecodeEntity" laws
  where
    roundTrip = property $ \(a :: a, entity :: Entity) -> decodeEntity entity (toByteString' a) == Right a
    laws = [("roundTrip", roundTrip)]

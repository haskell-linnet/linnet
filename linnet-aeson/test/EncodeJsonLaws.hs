{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module EncodeJsonLaws
  ( encodeJsonLaws
  ) where

import qualified Data.Aeson              as Aeson
import           Linnet.ContentTypes     (ApplicationJson)
import           Linnet.Encode
import           Test.QuickCheck         (Arbitrary, property)
import           Test.QuickCheck.Classes (Laws (..))

encodeJsonLaws ::
     forall a. (Encode ApplicationJson a, Arbitrary a, Show a, Aeson.FromJSON a, Eq a)
  => Laws
encodeJsonLaws = Laws "EncodeJSON" properties
  where
    roundTrip = property $ \(a :: a) -> (Aeson.decode . encode @ApplicationJson) a == Just a
    properties = [("roundTrip", roundTrip)]

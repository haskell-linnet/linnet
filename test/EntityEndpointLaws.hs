{-# LANGUAGE ScopedTypeVariables #-}

module EntityEndpointLaws
  ( entityEndpointLaws
  ) where

import           Linnet
import           Linnet.Decode           (DecodeEntity (..))
import           Linnet.Input            (Input (..))
import           Test.QuickCheck         (Arbitrary, property)
import           Test.QuickCheck.Classes (Laws (..))
import           Util
entityEndpointLaws ::
     forall a m. (DecodeEntity a, Eq (m (Maybe a)), Applicative m, Arbitrary a, Show a)
  => Endpoint m a
  -> (a -> Input)
  -> Laws
entityEndpointLaws endpoint serialize = Laws "EntityEndpoint" properties
  where
    roundTrip =
      property $ \(a :: a) ->
        let 
            i = serialize a
         in resultValueUnsafe (runEndpoint endpoint i) == pure (Just a)
    properties = [("roundTrip", roundTrip)]

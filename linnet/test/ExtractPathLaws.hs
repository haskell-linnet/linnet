{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ExtractPathLaws
  ( extractPathLaws
  ) where

import           Data.Data               (Proxy (..), Typeable, typeRep)
import           Data.Function           ((&))
import           Data.Functor.Identity   (Identity (..))
import           Data.Maybe              (isNothing, maybeToList)
import           Data.Text               (pack)
import           Instances
import           Linnet
import           Linnet.Decode
import           Linnet.Endpoint         (maybeReminder, maybeTrace)
import           Linnet.Input            (Input (..))
import           Test.QuickCheck         (Arbitrary, property)
import           Test.QuickCheck.Classes (Laws (..))
import           Util

extractPathLaws ::
     forall a. (DecodePath a, Arbitrary a, Typeable a, Eq a)
  => Laws
extractPathLaws = Laws "ExtractPath" properties
  where
    one = path @a
    tail' = paths @a
    extractOne =
      property $ \(i :: Input) ->
        let result = runEndpoint one i
            v = (headOption . reminder) i >>= decodePath @a
         in resultValueUnsafe result == Identity v &&
            (isNothing v ||
             (maybeReminder result == Just i {reminder = (tail . reminder) i} &&
              maybeTrace result == Just [pack $ show (typeRep (Proxy :: Proxy a))]))
    extractTail =
      property $ \(i :: Input) ->
        let result = runEndpoint tail' i
         in resultValueUnsafe result == Identity (Just $ reminder i >>= maybeToList . decodePath @a) &&
            maybeReminder result == Just i {reminder = []}
    properties = [("extractOne", extractOne), ("extractTail", extractTail)]

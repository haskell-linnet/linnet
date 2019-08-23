{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module DecodeJsonLaws
  ( decodeJsonLaws
  ) where

import qualified Data.Aeson              as Aeson
import           Data.ByteString.Lazy
import           Linnet                  (ApplicationJson)
import           Linnet.Decode
import           Linnet.Errors           (LinnetError (..))
import           Test.QuickCheck         (Arbitrary, property)
import           Test.QuickCheck.Classes (Laws (..))

decodeJsonLaws ::
     forall a. (Decode ApplicationJson a, Arbitrary a, Show a, Eq a, Aeson.ToJSON a)
  => Laws
decodeJsonLaws = Laws "DecodeJSON" laws
  where
    roundTrip = property $ \(a :: a) -> decode @ApplicationJson (Aeson.encode a) == Right a
    errorHandle =
      property $ \(a :: a) ->
        case decode @ApplicationJson @a ("@" `append` Aeson.encode a) of
          Left (DecodeError err) -> True
    laws = [("roundTrip", roundTrip), ("errorHandle", errorHandle)]

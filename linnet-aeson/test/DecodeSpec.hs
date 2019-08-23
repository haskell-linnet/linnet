{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module DecodeSpec
  ( spec
  ) where

import           Data.ByteString           (ByteString)
import           Data.Text                 (Text)
import           DecodeJsonLaws
import           Foo
import           Linnet.Aeson
import           Test.Hspec
import           Test.QuickCheck           (property)
import           Test.QuickCheck.Instances
import           Util

spec :: Spec
spec = do
  checkLaws "Int" $ decodeJsonLaws @Int
  checkLaws "Text" $ decodeJsonLaws @Text
  checkLaws "Bool" $ decodeJsonLaws @Bool
  checkLaws "Foo" $ decodeJsonLaws @Foo
  checkLaws "Maybe Foo" $ decodeJsonLaws @(Maybe Foo)
  checkLaws "[Foo]" $ decodeJsonLaws @[Foo]

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module EncodeSpec
  ( spec
  ) where

import           Data.ByteString           (ByteString)
import           Data.Text                 (Text)
import           EncodeJsonLaws
import           Foo
import           Linnet.Aeson
import           Test.Hspec
import           Test.QuickCheck           (property)
import           Test.QuickCheck.Instances
import           Util

spec :: Spec
spec = do
  checkLaws "Int" $ encodeJsonLaws @Int
  checkLaws "Text" $ encodeJsonLaws @Text
  checkLaws "Bool" $ encodeJsonLaws @Bool
  checkLaws "Foo" $ encodeJsonLaws @Foo
  checkLaws "Maybe Foo" $ encodeJsonLaws @(Maybe Foo)
  checkLaws "[Foo]" $ encodeJsonLaws @[Foo]

{-# LANGUAGE TypeApplications #-}

module EncodeSpec
  ( spec
  ) where

import           EncodeLaws
import           Test.Hspec
import           Test.QuickCheck.Instances
import           Util

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL

spec :: Spec
spec = do
  checkLaws "Text" (encodeTextLaws @T.Text)
  checkLaws "Text.Lazy" (encodeTextLaws @TL.Text)
  checkLaws "ByteString" (encodeTextLaws @BS.ByteString)
  checkLaws "ByteString.Lazy" (encodeTextLaws @BL.ByteString)
  checkLaws "Int" (encodeTextLaws @Int)
  checkLaws "Double" (encodeTextLaws @Double)

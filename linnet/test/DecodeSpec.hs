{-# LANGUAGE TypeApplications #-}

module DecodeSpec
  ( spec
  ) where

import qualified Data.ByteString           as BS
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           DecodeLaws
import           Instances
import           Test.Hspec                (Spec, describe)
import           Test.QuickCheck.Instances
import           Util

spec :: Spec
spec = do
  checkLaws "Text" $ decodePathLaws @T.Text id
  checkLaws "Integer" $ decodePathLaws @Integer (T.pack . show)
  checkLaws "Int" $ decodePathLaws @Int (T.pack . show)
  checkLaws "Text" $ decodeEntityLaws @T.Text
  checkLaws "Integer" $ decodeEntityLaws @Integer
  checkLaws "Int" $ decodeEntityLaws @Int

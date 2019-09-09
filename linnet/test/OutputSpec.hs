{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OutputSpec where

import           Data.Data               (Proxy (..))
import           Instances
import           Linnet                  (Output)
import           Test.Hspec
import           Test.QuickCheck.Classes (applicativeLaws, foldableLaws,
                                          functorLaws, monadLaws)
import           Util

spec :: Spec
spec =
  describe "OutputSpec" $ do
    checkLaws "Output" $ functorLaws (Proxy :: Proxy Output)
    checkLaws "Output" $ foldableLaws (Proxy :: Proxy Output)
  --checkLaws "Output" $ monadLaws (Proxy :: Proxy Output)
  --checkLaws "Output" $ applicativeLaws (Proxy :: Proxy Output)

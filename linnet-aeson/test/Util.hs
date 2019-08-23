module Util
  ( checkLaws
  ) where

import           Test.Hspec
import           Test.QuickCheck.Classes (Laws, lawsProperties, lawsTypeclass)

checkLaws :: String -> Laws -> SpecWith ()
checkLaws name laws = describe (lawsTypeclass laws ++ " @" ++ name) properties
  where
    properties :: SpecWith ()
    properties = mapM_ (uncurry it) (lawsProperties laws)

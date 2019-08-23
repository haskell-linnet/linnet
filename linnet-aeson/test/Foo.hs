{-# LANGUAGE DeriveGeneric #-}

module Foo
  ( Foo(..)
  ) where

import           Data.Aeson
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))

data Foo =
  Foo
    { x :: String
    , y :: Int
    , z :: Maybe Bool
    }
  deriving (Generic, Eq, Show)

instance ToJSON Foo

instance FromJSON Foo

instance Arbitrary Foo where
  arbitrary = Foo <$> arbitrary <*> arbitrary <*> arbitrary

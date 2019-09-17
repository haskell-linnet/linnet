{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Foo
  ( Foo(..)
  , fooFromChar
  ) where

import           Control.DeepSeq      (NFData)
import qualified Data.Aeson           as Aeson
import           Data.ByteString.Lazy (fromChunks, fromStrict, toStrict)
import           Data.Text            (Text, intercalate, singleton)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Linnet
import           Linnet.Endpoint      (EndpointResult)

newtype Foo =
  Foo
    { foo :: Text
    }
  deriving (Aeson.FromJSON, Aeson.ToJSON, NFData)

fooFromChar :: Char -> Foo
fooFromChar = Foo . singleton

instance Decode TextPlain Foo where
  decode = Right . Foo . decodeUtf8 . toStrict

instance Encode TextPlain [Foo] where
  encode = fromChunks . map (encodeUtf8 . foo)

instance Semigroup Foo where
  (<>) a b = Foo (foo a <> foo b)

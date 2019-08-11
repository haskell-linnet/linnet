{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Linnet.Encode
  ( Encode
  , encode
  ) where

import           Data.ByteString         as BS
import           Data.ByteString.Lazy    as BL
import           Data.Text               as TS
import           Data.Text.Encoding      as TSE
import           Data.Text.Lazy          as TL
import           Data.Text.Lazy.Encoding as TLE
import           GHC.Base                (Symbol)
import           Linnet.ContentTypes

class Encode (ct :: Symbol) a where
  encode :: a -> BL.ByteString

instance Encode TextPlain BL.ByteString where
  encode = id

instance Encode TextPlain BS.ByteString where
  encode = BL.fromStrict

instance Encode TextPlain TS.Text where
  encode = BL.fromStrict . TSE.encodeUtf8

instance Encode TextPlain TL.Text where
  encode = TLE.encodeUtf8

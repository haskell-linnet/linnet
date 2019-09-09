{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Linnet.Encode
  ( Encode
  , encode
  ) where

import           Data.ByteString            as BS
import           Data.ByteString.Conversion as BC
import           Data.ByteString.Lazy       as BL
import           Data.Text                  as TS
import           Data.Text.Encoding         as TSE
import           Data.Text.Lazy             as TL
import           Data.Text.Lazy.Encoding    as TLE
import           Linnet.ContentTypes

-- | Encoding of some type @a@ into payload of HTTP response
-- Phantom type @ct@ guarantees that compiler checks support of encoding of some @a@ into content of given @Content-Type@
-- by looking for specific @Encode@ instance.
class Encode ct a where
  encode :: a -> BL.ByteString

instance Encode TextPlain BL.ByteString where
  encode = id

instance Encode TextPlain BS.ByteString where
  encode = BL.fromStrict

instance Encode TextPlain TS.Text where
  encode = BL.fromStrict . TSE.encodeUtf8

instance Encode TextPlain TL.Text where
  encode = TLE.encodeUtf8

instance Encode TextPlain Int where
  encode = BC.toByteString

instance Encode TextPlain Integer where
  encode = BC.toByteString

instance Encode TextPlain Double where
  encode = BC.toByteString

instance Encode TextPlain Float where
  encode = BC.toByteString

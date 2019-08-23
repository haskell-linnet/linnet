{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-|
Module: Linnet Aeson
Description: Aeson JSON support for Linnet
Copyright: (c) Sergey Kolbasov, 2019
License: Apache License 2.0

This package adds support of JSON requests & responses in Linnet library
using <http://hackage.haskell.org/package/aeson aeson>.

See the detailed documentation on <http://linnet.io linnet.io>.

-}
module Linnet.Aeson where

import           Control.Arrow         (left)
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString.Char8 as C8
import           Linnet.ContentTypes   (ApplicationJson)
import           Linnet.Decode
import           Linnet.Encode
import           Linnet.Errors

instance {-# OVERLAPPABLE #-} Aeson.ToJSON a => Encode ApplicationJson a where
  encode = Aeson.encode

instance {-# OVERLAPPABLE #-} Aeson.FromJSON a => Decode ApplicationJson a where
  decode = left (DecodeError . C8.pack) . Aeson.eitherDecode

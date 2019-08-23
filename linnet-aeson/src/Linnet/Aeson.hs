{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Linnet.Aeson where

import           Control.Arrow         (left)
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString.Char8 as C8
import           Data.Function         ((&))
import           Linnet.ContentTypes   (ApplicationJson)
import           Linnet.Decode
import           Linnet.Encode
import           Linnet.Errors

instance {-# OVERLAPPABLE #-} Aeson.ToJSON a => Encode ApplicationJson a where
  encode = Aeson.encode

instance {-# OVERLAPPABLE #-} Aeson.FromJSON a => Decode ApplicationJson a where
  decode = left (DecodeError . C8.pack) . Aeson.eitherDecode

module Linnet.Endpoints.Entity
  ( Entity(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Text       as T

data Entity
  = Param
      { entityName :: B.ByteString
      }
  | Header
      { entityName :: B.ByteString
      }
  | Body
  | Cookie
      { entityName :: B.ByteString
      }
  deriving (Eq, Show)

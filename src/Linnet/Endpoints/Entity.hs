module Linnet.Endpoints.Entity
  ( Entity(..)
  ) where

import qualified Data.ByteString as B

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

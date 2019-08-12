module Linnet.Errors
  ( LinnetError(..)
  ) where

import           Control.Exception       (Exception)
import qualified Data.ByteString         as B
import           Data.List.NonEmpty      (NonEmpty)
import           Linnet.Endpoints.Entity

data LinnetError
  = DecodeEntityError
      { nondecodableEntity :: Entity
      , decodeEntityError  :: LinnetError
      }
  | MissingEntity
      { missingEntity :: Entity
      }
  | EntityNotParsed
      { notParsedEntity    :: Entity
      , entityParsingError :: LinnetError
      }
  | LinnetErrors
      { linnetErrors :: NonEmpty LinnetError
      }
  | DecodeError
      { decodeError :: B.ByteString
      }
  deriving (Eq, Show)

instance Exception LinnetError

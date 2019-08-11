module Linnet.Errors
  ( LinnetError(..)
  ) where

import           Control.Exception  (Exception)
import qualified Data.ByteString    as B
import           Data.List.NonEmpty (NonEmpty)

data LinnetError
  = DecodeEntityError
      { decodeEntityError :: B.ByteString
      }
  | MissingEntity
      { missingEntityName :: B.ByteString
      }
  | EntityNotParsed
      { notParsedEntityName :: B.ByteString
      , entityParsingError  :: LinnetError
      }
  | LinnetErrors
      { linnetErrors :: NonEmpty LinnetError
      }
  | DecodeError
      { decodeError :: B.ByteString
      }
  deriving (Eq, Show)

instance Exception LinnetError

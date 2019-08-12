module Linnet.Errors
  ( LinnetError(..)
  ) where

import           Control.Exception       (Exception)
import qualified Data.ByteString         as B
import           Data.List.NonEmpty      (NonEmpty (..), toList, (<|))
import           Linnet.Endpoints.Entity

data LinnetError
  = MissingEntity
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

instance Semigroup LinnetError where
  (LinnetErrors es) <> (LinnetErrors es') =
    let (h:t) = toList es
        (h':t') = toList es
     in LinnetErrors $ h :| (t ++ [h] ++ t')
  (LinnetErrors es) <> err = LinnetErrors $ err <| es
  err <> (LinnetErrors es) = LinnetErrors $ err <| es
  e <> e' = LinnetErrors $ e :| [e']

instance Exception LinnetError

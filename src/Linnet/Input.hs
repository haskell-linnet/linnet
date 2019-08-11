module Linnet.Input
  ( Input(..)
  , inputFromGet
  ) where

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Network.Wai        (Request, defaultRequest, pathInfo,
                                     rawPathInfo)

-- | Container for the reminder of the request path and request itself
data Input =
  Input
    { reminder :: [T.Text]
    , request  :: Request
    }
  deriving (Show)

inputFromGet :: T.Text -> Input
inputFromGet path = Input {request = defaultRequest {pathInfo = rem, rawPathInfo = TE.encodeUtf8 path}, reminder = rem}
  where
    rem = T.split (== '/') path

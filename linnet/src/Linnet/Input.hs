module Linnet.Input
  ( Input(..)
  , inputGet
  , inputFromRequest
  ) where

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Types (QueryItem)
import           Network.Wai        (Request, defaultRequest, pathInfo,
                                     queryString, rawPathInfo)

-- | Container for the reminder of the request path and request itself
data Input =
  Input
    { reminder :: [T.Text]
    , request  :: Request
    }
  deriving (Show)

inputFromRequest :: Request -> Input
inputFromRequest req = Input {reminder = pathInfo req, request = req}

inputGet :: T.Text -> [QueryItem] -> Input
inputGet path items =
  Input {request = defaultRequest {pathInfo = r, rawPathInfo = TE.encodeUtf8 path, queryString = items}, reminder = r}
  where
    r = T.split (== '/') path

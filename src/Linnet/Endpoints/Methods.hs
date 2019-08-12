module Linnet.Endpoints.Methods
  ( get
  , post
  , put
  , patch
  , delete
  , head'
  , trace'
  , connect
  , options
  ) where

import           Linnet.Endpoint
import           Linnet.Input
import           Network.HTTP.Types
import           Network.Wai        (requestMethod)

methodEndpoint :: Method -> Endpoint m a -> Endpoint m a
methodEndpoint method underlying =
  Endpoint
    { runEndpoint =
        \input ->
          if (requestMethod . request) input == method
            then runEndpoint underlying input
            else NotMatched
    , toString = show method ++ " " ++ toString underlying
    }

-- | Turn endpoint into one that matches only for GET requests
get :: Endpoint m a -> Endpoint m a
get = methodEndpoint methodGet

-- | Turn endpoint into one that matches only for POST requests
post :: Endpoint m a -> Endpoint m a
post = methodEndpoint methodPost

-- | Turn endpoint into one that matches only for PUT requests
put :: Endpoint m a -> Endpoint m a
put = methodEndpoint methodPut

-- | Turn endpoint into one that matches only for PATCH requests
patch :: Endpoint m a -> Endpoint m a
patch = methodEndpoint methodPatch

-- | Turn endpoint into one that matches only for DELETE requests
delete :: Endpoint m a -> Endpoint m a
delete = methodEndpoint methodDelete

-- | Turn endpoint into one that matches only for HEAD requests
head' :: Endpoint m a -> Endpoint m a
head' = methodEndpoint methodHead

-- | Turn endpoint into one that matches only for TRACE requests
trace' :: Endpoint m a -> Endpoint m a
trace' = methodEndpoint methodTrace

-- | Turn endpoint into one that matches only for CONNECT requests
connect :: Endpoint m a -> Endpoint m a
connect = methodEndpoint methodConnect

-- | Turn endpoint into one that matches only for OPTIONS requests
options :: Endpoint m a -> Endpoint m a
options = methodEndpoint methodOptions

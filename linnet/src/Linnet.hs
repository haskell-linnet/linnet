{-|
Module: Linnet
Description: Lightweight HTTP library on top of WAI
Copyright: (c) Sergey Kolbasov, 2019
License: Apache License 2.0

Linnet [ˈlɪnɪt] is a lightweight Haskell library for building HTTP API on top of <http://hackage.haskell.org/package/wai WAI>.
Library design is heavily inspired by Scala <https://github.com/finagle/finch Finch>.

See the detailed documentation on <http://linnet.io linnet.io>.

-}
module Linnet
  (
  -- * Hello world
  -- $helloWorld
  -- * Endpoint and core combinators
   Endpoint(..)
  , (~>)
  , (~>>)
  , (//)
  , (|+|)
  -- ** Method endpoints
  , get
  , post
  , put
  , patch
  , delete
  , head'
  , trace'
  , connect
  , options
  -- ** Path matching endpoints
  , path
  , pathAny
  , pathConst
  , p'
  , pathEmpty
  , paths
  -- ** Query parameters endpoints
  , param
  , paramMaybe
  , params
  , paramsNel
  -- ** Request body endpoints
  , body
  , bodyMaybe
  , textBody
  , textBodyMaybe
  , jsonBody
  , jsonBodyMaybe
  -- ** Cookie endpoints
  , cookie
  , cookieMaybe
  -- ** Header endpoints
  , header
  , headerMaybe
  -- ** Response encoding and request decoding
  , Encode(..)
  , Decode(..)
  -- ** Endpoint output
  , Output(..)
  , ok
  , created
  , accepted
  , noContent
  , badRequest
  , unauthorized
  , paymentRequired
  , forbidden
  , notFound
  , methodNotAllowed
  , notAcceptable
  , conflict
  , gone
  , lengthRequired
  , preconditionFailed
  , requestEntityTooLarge
  , unprocessableEntity
  , tooManyRequests
  , internalServerError
  , notImplemented
  , badGateway
  , serviceUnavailable
  , gatewayTimeout
  -- * Compiling an endpoint
  , bootstrap
  , serve
  , compile
  , toApp
  -- * Running a server
  , run
  -- * Content-Type literals
  , ApplicationJson
  , TextHtml
  , TextPlain
  -- * Content-Type negotiation
  , NotAcceptable406
  , (:+:)
  ) where

import           Linnet.Bootstrap
import           Linnet.ContentTypes
import           Linnet.Decode
import           Linnet.Encode
import           Linnet.Endpoint
import           Linnet.Endpoints
import           Linnet.Internal.Coproduct ((:+:))
import           Linnet.Output
import           Linnet.ToResponse         (NotAcceptable406)
import           Network.Wai.Handler.Warp
-- $helloWorld
-- Hello @name@ example using warp server:
--
--  > {-# LANGUAGE FlexibleInstances      #-}
--  > {-# LANGUAGE MultiParamTypeClasses  #-}
--  > {-# LANGUAGE OverloadedStrings      #-}
--  > {-# LANGUAGE TypeApplications       #-}
--  > {-# LANGUAGE TypeSynonymInstances   #-}
--  >
--  > import Control.Exception (SomeException)
--  > import Data.Function ((&))
--  > import Data.Text (Text, append)
--  > import Linnet
--  >
--  > -- It's necessary to define encoding of exceptions for content-type "text/plain". Here it returns no content
--  > instance Encode TextPlain SomeException where
--  >  encode _ = mempty
--  >
--  > helloWorld = get(p' "hello" // path @Text) ~>> (\name -> return $ ok ("Hello, " `append` name))
--  >
--  > main :: IO ()
--  > main = run 9000 $ bootstrap @TextPlain helloWorld & compile & toApp id
--
-- Now try to call your server with @curl@ command:
--
-- > curl -v http://localhost:9000/hello/linnet
--
-- __Main module exposes only subset of available functions and operators to keep application namespace clean.__
--
-- Explore corresponding modules for additional functionality.

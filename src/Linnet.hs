{-|
Module: Linnet
Description: Lightweight HTTP library on top of WAI
Copyright: (c) Sergey Kolbasov, 2019
License: Apache License 2.0

Linnet [ˈlɪnɪt] is a library that provides combinators for building HTTP API on top of <http://hackage.haskell.org/package/wai WAI>.
Library design is heavily inspired by Scala <https://github.com/finagle/finch Finch> made by V. Kostyukov.

Main module exposes only subset of available functions and operators.
-}
module Linnet
  ( ApplicationJson
  , TextHtml
  , TextPlain
  , Endpoint(..)
  , Encode(..)
  , Decode(..)
  , Output(..)
  , (~>)
  , (~>>)
  , (//)
  , (|+|)
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
  , cookie
  , cookieMaybe
  , header
  , headerMaybe
  , get
  , post
  , put
  , patch
  , delete
  , head'
  , trace'
  , connect
  , options
  , param
  , paramMaybe
  , params
  , paramsNel
  , path
  , pathAny
  , pathConst
  , p'
  , pathEmpty
  , paths
  , body
  , bodyMaybe
  , textBody
  , textBodyMaybe
  , jsonBody
  , jsonBodyMaybe
  , bootstrap
  , serve
  , compile
  , toApp
  ) where

import           Linnet.Bootstrap
import           Linnet.ContentTypes
import           Linnet.Decode
import           Linnet.Encode
import           Linnet.Endpoint
import           Linnet.Endpoints
import           Linnet.Output

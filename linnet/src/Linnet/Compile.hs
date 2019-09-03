{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Linnet.Compile
  ( Compile(..)
  ) where

import           Control.Exception         (SomeException)
import           Control.Monad             (join, (>=>))
import           Control.Monad.Catch       (MonadCatch)
import           Control.Monad.Reader      (ReaderT (..))
import           Data.ByteString           (intercalate)
import           Data.ByteString.Char8     (split)
import           Data.Maybe                (maybeToList)
import           Linnet.Endpoint
import           Linnet.Errors             (LinnetError)
import           Linnet.Input
import           Linnet.Internal.Coproduct
import           Linnet.Internal.HList
import           Linnet.Output             (Output (..), outputToResponse,
                                            payloadError)
import           Linnet.ToResponse         (Negotiable (..))
import           Network.HTTP.Media        (MediaType, parseQuality)
import           Network.HTTP.Types        (Method, badRequest400, hAccept,
                                            methodNotAllowed405, notFound404)
import           Network.HTTP.Types.Header (hAllow)
import           Network.Wai               (Request, Response, requestHeaders,
                                            responseLBS)

newtype CompileContext =
  CompileContext
    { allowedMethods :: [Method]
    }

class Compile cts m es where
  compile :: es -> ReaderT Request m Response

instance (Compile' cts m es) => Compile cts m es where
  compile es = compile' @cts es (CompileContext [])

class Compile' cts m es where
  compile' :: es -> CompileContext -> ReaderT Request m Response

instance (Monad m) => Compile' CNil m (HList '[]) where
  compile' _ CompileContext {..} =
    ReaderT $
    const
      (if null allowedMethods
         then notFoundResponse
         else methodNotAllowedResponse allowedMethods)

instance (Negotiable ct a, Negotiable ct SomeException, Negotiable ct (), Compile' cts m (HList es), MonadCatch m) =>
         Compile' (ct :+: cts) m (HList (Endpoint m a ': es)) where
  compile' (ea ::: es) ctx@CompileContext {..} =
    ReaderT
      (\req ->
         let accept =
               (maybeToList . lookup hAccept >=> split ',' >=> (join . maybeToList . parseQuality @MediaType)) .
               requestHeaders $
               req
          in case runEndpoint (handle respond400 ea) (inputFromRequest req) of
               Matched _ mo ->
                 outputToResponse
                   (negotiate @ct accept Nothing)
                   (negotiate @ct accept Nothing)
                   (negotiate @ct accept Nothing) <$>
                 mo
               NotMatched r ->
                 let newContext =
                       case r of
                         MethodNotAllowed allowed -> ctx {allowedMethods = allowed : allowedMethods}
                         Other -> ctx
                  in runReaderT (compile' @cts es newContext) req)

notFoundResponse :: (Applicative m) => m Response
notFoundResponse = pure $ responseLBS notFound404 [] mempty

methodNotAllowedResponse :: (Applicative m) => [Method] -> m Response
methodNotAllowedResponse wouldAllow = pure $ responseLBS methodNotAllowed405 [(hAllow, headerValue)] mempty
  where
    headerValue = intercalate ", " wouldAllow

respond400 :: (Applicative m) => LinnetError -> m (Output a)
respond400 err = pure $ payloadError badRequest400 err

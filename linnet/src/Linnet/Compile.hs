{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Linnet.Compile
  ( Compile(..)
  ) where

import           Control.Exception         (SomeException)
import           Control.Monad.Catch       (MonadCatch)
import           Control.Monad.Reader      (ReaderT(..))
import           Data.Data                 (Proxy)
import           GHC.TypeLits              (KnownSymbol)
import           Linnet.Endpoint
import           Linnet.Errors             (LinnetError)
import           Linnet.Input
import           Linnet.Internal.Coproduct
import           Linnet.Internal.HList
import           Linnet.Output             (Output (..), outputToResponse,
                                            payloadError)
import           Linnet.ToResponse         (ToResponse)
import           Network.HTTP.Types        (badRequest400, status404)
import           Network.Wai               (Request, Response, responseLBS)

class Compile cts m es where
  compile :: es -> ReaderT Request m Response

instance (Monad m) => Compile CNil m (HList '[]) where
  compile _ = ReaderT $ const notFoundResponse

instance (KnownSymbol ct, ToResponse ct a, ToResponse ct SomeException, Compile cts m (HList es), MonadCatch m) =>
         Compile (Coproduct (Proxy ct) cts) m (HList (Endpoint m a ': es)) where
  compile (ea ::: es) =
    ReaderT
      (\req ->
         case runEndpoint (handle respond400 ea) (inputFromRequest req) of
           Matched _ mo -> outputToResponse @a @ct <$> mo
           NotMatched   -> runReaderT (compile @cts es) req)

notFoundResponse :: (Applicative m) => m Response
notFoundResponse = pure $ responseLBS status404 [] mempty

respond400 :: (Applicative m) => LinnetError -> m (Output a)
respond400 err = pure $ payloadError badRequest400 err
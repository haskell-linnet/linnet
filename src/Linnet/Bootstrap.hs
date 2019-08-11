{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Linnet.Bootstrap
  ( bootstrap
  , serve
  , compile
  , toApp
  ) where

import           Control.Arrow             (Kleisli (..))
import           Control.Monad.Base        (MonadBase (..))
import           Data.Data                 (Proxy)
import           Data.Functor.Identity     (Identity)
import           GHC.Base                  (Symbol)
import qualified Linnet.Compile            as Compile
import           Linnet.Endpoint
import           Linnet.Internal.Coproduct (CNil, Coproduct)
import           Linnet.Internal.HList     (HList (..))
import           Network.Wai               (Application, Request, Response)

newtype Bootstrap (m :: * -> *) cts es =
  Bootstrap es

bootstrap ::
     forall (ct :: Symbol) m a. Endpoint m a -> Bootstrap m (Coproduct (Proxy ct) CNil) (HList '[ (Endpoint m a)])
bootstrap ea = Bootstrap @m @(Coproduct (Proxy ct) CNil) (ea ::: HNil)

serve ::
     forall (ct :: Symbol) cts es m a.
     Endpoint m a
  -> Bootstrap m cts (HList es)
  -> Bootstrap m (Coproduct (Proxy ct) cts) (HList (Endpoint m a ': es))
serve ea (Bootstrap e) = Bootstrap @m @(Coproduct (Proxy ct) cts) (ea ::: e)

compile ::
     forall cts m es. (Compile.Compile cts m es)
  => Bootstrap m cts es
  -> Kleisli m Request Response
compile (Bootstrap e) = Compile.compile @cts @m e

toApp :: (m Response -> IO Response) -> Kleisli m Request Response -> Application
toApp toIO kleisli request callback = toIO (runKleisli kleisli request) >>= callback

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Linnet.Bootstrap
  ( bootstrap
  , serve
  , compile
  , toApp
  ) where

import           Control.Monad.Reader         (ReaderT (..))
import           Data.Data                    (Proxy)
import           GHC.Base                     (Symbol)
import qualified Linnet.Compile               as Compile
import           Linnet.Endpoint
import           Linnet.Internal.Coproduct    (CNil, Coproduct)
import           Linnet.Internal.HList        (HList (..))
import           Linnet.NaturalTransformation
import           Network.Wai                  (Application, Request, Response)

newtype Bootstrap (m :: * -> *) cts es =
  Bootstrap es

-- | Create 'Bootstrap' out of single 'Endpoint' and some given Content-Type:
--
-- > bootstrap @TextPlain (pure "foo")
bootstrap ::
     forall (ct :: Symbol) m a. Endpoint m a -> Bootstrap m (Coproduct (Proxy ct) CNil) (HList '[ (Endpoint m a)])
bootstrap ea = Bootstrap @m @(Coproduct (Proxy ct) CNil) (ea ::: HNil)

-- | Add another endpoint to 'Bootstrap' for purpose of serving multiple Content-Types with *different* endpoints
--
-- > bootstrap @TextPlain (pure "foo") & server @ApplicationJson (pure "bar")
serve ::
     forall (ct :: Symbol) cts es m a.
     Endpoint m a
  -> Bootstrap m cts (HList es)
  -> Bootstrap m (Coproduct (Proxy ct) cts) (HList (Endpoint m a ': es))
serve ea (Bootstrap e) = Bootstrap @m @(Coproduct (Proxy ct) cts) (ea ::: e)

-- | Compile 'Bootstrap' into @ReaderT Request m Response@ for further combinations.
-- Might be useful to implement middleware in context of the same monad @m@:
--
-- > bootstrap @TextPlain (pure "foo") & compile
compile ::
     forall cts m es. (Compile.Compile cts m es)
  => Bootstrap m cts es
  -> ReaderT Request m Response
compile (Bootstrap e) = Compile.compile @cts @m e

-- | Convert @ReaderT Request m Response@ into WAI @Application@
--
-- > bootstrap @TextPlain (pure "foo") & compile & toApp id
--
-- The first parameter here is a natural transformation of 'Endpoint's monad @m@ into @IO@.
-- In case if selected monad is @IO@ already then @id@ is just enough. Otherwise, it's a good place to define how to "start"
-- custom monad for each request to come and convert it to @IO@.
--
-- As an example:
--
--  * @ReaderT RequestContext IO@ could be used to pass some data as local context for the request.
--
--  * Some monad for logging (i.e. co-log)
toApp :: forall m . (NaturalTransformation m IO) => ReaderT Request m Response -> Application
toApp !readerT request callback = mapK (runReaderT readerT request) >>= callback

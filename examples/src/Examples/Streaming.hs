{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Examples.Streaming
  ( app
  ) where

import           Conduit            (ConduitT, lift, yield)
import           Control.Concurrent (threadDelay)
import           Control.Exception  (SomeException)
import           Data.Aeson.Types   (ToJSON)
import           Data.Function      ((&))
import           Data.IORef         (IORef, atomicModifyIORef, readIORef,
                                     writeIORef)
import           Data.Text          (Text, append)
import           GHC.Generics       (Generic)
import           Linnet
import           Linnet.Aeson
import           Linnet.Conduit
import           Network.Wai        (Application)

instance Encode TextPlain SomeException where
  encode _ = mempty

monotonicStream :: IORef Counter -> ConduitT () Counter IO ()
monotonicStream counter = do
  value <- lift $ atomicModifyIORef counter (\c@Counter {..} -> (c {value = value + 1}, c))
  yield value
  lift $ threadDelay 1000000
  monotonicStream counter

streamCounter :: IORef Counter -> Endpoint IO (ConduitT () Counter IO ())
streamCounter counter = get (p' "streaming") ~>> return (ok $ monotonicStream counter)

app :: IORef Counter -> Application
app counter = bootstrap @ApplicationJson (streamCounter counter) & compile & toApp @IO

newtype Counter =
  Counter
    { value :: Int
    }
  deriving (Generic, ToJSON)

instance Semigroup Counter where
  c <> c' = Counter $ value c + value c'

instance Monoid Counter where
  mempty = Counter 0

instance Encode ApplicationJson SomeException where
  encode _ = mempty

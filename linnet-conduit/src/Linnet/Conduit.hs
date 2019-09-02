{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-|
Module: Linnet Conduit
Description: Streaming support for Linnet
Copyright: (c) Sergey Kolbasov, 2019
License: Apache License 2.0

This package adds support of streaming requests & responses in Linnet library using <http://hackage.haskell.org/package/conduit conduit>.

See the detailed documentation on <http://linnet.io linnet.io>.

-}
module Linnet.Conduit
  ( streamBody
  ) where

import           Conduit                      (ConduitT, mapM_C, runConduit,
                                               yield, (.|))
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (MonadIO (..))
import qualified Data.ByteString.Builder      as Builder
import qualified Data.ByteString.Char8        as C8
import qualified Data.ByteString.Lazy         as BL
import           Data.Data                    (Proxy (..))
import           Data.Void                    (Void)
import           GHC.TypeLits                 (KnownSymbol, symbolVal)
import           Linnet
import           Linnet.Endpoint              (EndpointResult (..), NotMatchedReason(..))
import           Linnet.Input                 (request)
import           Linnet.NaturalTransformation (NaturalTransformation (..))
import           Linnet.ToResponse
import           Network.HTTP.Types           (status200)
import           Network.Wai                  (RequestBodyLength (..),
                                               lazyRequestBody,
                                               requestBodyLength,
                                               responseStream)
import           Network.Wai.Handler.Warp     (pauseTimeout)

fromLazyBody :: (MonadIO m) => IO BL.ByteString -> ConduitT i BL.ByteString m ()
fromLazyBody reader = do
  bs <- liftIO reader
  unless (bs == BL.empty) $ yield bs >> fromLazyBody reader

-- | Endpoint that reads chunked request body as 'ConduitT' stream of lazy @ByteString@. Matches if body is chunked.
-- Beware that it also pauses slowloris timeout in Warp as it reads request body using lazy IO.
streamBody ::
     forall m i. (MonadIO m)
  => Endpoint m (ConduitT i BL.ByteString m ())
streamBody =
  Endpoint
    { runEndpoint =
        \input ->
          let req = request input
           in case requestBodyLength req of
                ChunkedBody ->
                  Matched
                    { matchedReminder = input
                    , matchedOutput =
                        do liftIO $ pauseTimeout req
                           pure $ ok $ (fromLazyBody . lazyRequestBody) req
                    }
                KnownLength _ -> NotMatched Other
    , toString = "streamBody"
    }

instance {-# OVERLAPS #-} (Encode ApplicationJson a, NaturalTransformation m IO, MonadIO m) =>
                          ToResponse ApplicationJson (ConduitT () a m ()) where
  toResponse stream =
    responseStream status200 [("Content-Type", "application/json")] $ \write flush ->
      let push :: ConduitT a Void m ()
          push =
            mapM_C
              (\chunk -> liftIO $ write (Builder.lazyByteString (encode @ApplicationJson chunk)) >> write "\n" >> flush)
       in mapK (runConduit $ stream .| push)

instance {-# OVERLAPS #-} (Encode ct a, KnownSymbol ct, NaturalTransformation m IO, MonadIO m) =>
                          ToResponse ct (ConduitT () a m ()) where
  toResponse stream =
    responseStream status200 [("Content-Type", C8.pack $ symbolVal (Proxy :: Proxy ct))] $ \write flush ->
      let push :: ConduitT a Void m ()
          push = mapM_C (\chunk -> liftIO $ write (Builder.lazyByteString (encode @ct chunk)) >> flush)
       in mapK (runConduit $ stream .| push)

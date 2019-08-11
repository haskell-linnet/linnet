{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util
  ( resultValueUnsafe
  , resultOutputEither
  , resultOutputUnsafe
  , headOption
  ) where

import           Control.Exception   (Exception (..), SomeException (..))
import           Control.Monad.Catch
import           Linnet.Endpoint     (EndpointResult (..))
import           Linnet.Output

headOption :: [a] -> Maybe a
headOption []    = Nothing
headOption (h:t) = Just h

resultOutputUnsafe :: (Applicative m) => EndpointResult m a -> m (Maybe (Output a))
resultOutputUnsafe (Matched _ m) = fmap Just m
resultOutputUnsafe NotMatched    = pure Nothing

resultValueUnsafe :: (Applicative m) => EndpointResult m a -> m (Maybe a)
resultValueUnsafe (Matched _ m) =
  fmap
    (\case
       Output _ (Payload a) _ -> Just a
       _ -> Nothing)
    m
resultValueUnsafe NotMatched = pure Nothing

resultOutputEither :: (MonadCatch m) => EndpointResult m a -> m (Either SomeException (Maybe (Output a)))
resultOutputEither endpointResult =
  case endpointResult of
    NotMatched -> pure $ Right Nothing
    Matched {matchedOutput = m} -> catchAll (fmap (Right . Just) m) (pure . Left)


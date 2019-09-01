{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util
  ( resultValueUnsafe
  , resultOutputEither
  , resultOutputUnsafe
  , headOption
  , checkLaws
  ) where

import           Control.Exception       (Exception (..), SomeException (..))
import           Control.Monad.Catch
import           Linnet.Endpoint         (EndpointResult (..))
import           Linnet.Output
import           Test.Hspec              (Example (..), SpecWith, describe, it)
import           Test.QuickCheck         (Property, property)
import           Test.QuickCheck.Classes (Laws (..))

headOption :: [a] -> Maybe a
headOption []    = Nothing
headOption (h:t) = Just h

resultOutputUnsafe :: (Applicative m) => EndpointResult m a -> m (Maybe (Output a))
resultOutputUnsafe (Matched _ m)  = fmap Just m
resultOutputUnsafe (NotMatched _) = pure Nothing

resultValueUnsafe :: (Applicative m) => EndpointResult m a -> m (Maybe a)
resultValueUnsafe (Matched _ m) =
  fmap
    (\case
       Output _ (Payload a) _ -> Just a
       _ -> Nothing)
    m
resultValueUnsafe (NotMatched _) = pure Nothing

resultOutputEither :: (MonadCatch m) => EndpointResult m a -> m (Either SomeException (Maybe (Output a)))
resultOutputEither endpointResult =
  case endpointResult of
    NotMatched _ -> pure $ Right Nothing
    Matched {matchedOutput = m} -> catchAll (fmap (Right . Just) m) (pure . Left)

checkLaws :: String -> Laws -> SpecWith ()
checkLaws name laws = describe (lawsTypeclass laws ++ " @" ++ name) properties
  where
    properties :: SpecWith ()
    properties = mapM_ (uncurry it) (lawsProperties laws)

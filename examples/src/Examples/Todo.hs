{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Examples.Todo
  ( Todo(..)
  ) where

import           Data.Maybe    (maybe)
import           Data.Text     (Text)

import           Data.Aeson
import           Data.Function ((&))

data Todo =
  Todo
    { todoTitle     :: Text
    , todoId        :: Int
    , todoCompleted :: Bool
    }
  deriving (Eq, Show)

patch :: Maybe Text -> Maybe Bool -> Todo -> Todo
patch maybeText maybeCompleted todo =
  case (maybeText, maybeCompleted) of
    (Just title, maybeCompleted) -> patch Nothing maybeCompleted $ todo {todoTitle = title}
    (_, Just completed) -> todo {todoCompleted = completed}
    _ -> todo

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \v -> Todo <$> v .: "title" <*> v .: "id" <*> v .: "completed"

instance ToJSON Todo where
  toJSON Todo {..} = object [("id", toJSON todoId), ("title", toJSON todoTitle), ("completed", toJSON todoCompleted)]

instance FromJSON (Int -> Bool -> Todo) where
  parseJSON = withObject "NewTodo" $ \v -> Todo <$> v .: "title"

instance FromJSON (Todo -> Todo) where
  parseJSON =
    withObject
      "PatchTodo"
      (\v -> do
         maybeTitle <- v .:? "title"
         maybeCompleted <- v .:? "completed"
         return $ patch maybeTitle maybeCompleted)

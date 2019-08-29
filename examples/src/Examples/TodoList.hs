{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Examples.TodoList
  ( app
  ) where

import           Control.Exception         (SomeException)
import           Data.Function             ((&))
import           Data.IORef                (IORef, atomicModifyIORef, readIORef)
import           Data.List                 (sortOn)
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text, append)
import           Examples.Todo
import           GHC.Generics              (Generic)
import           Linnet
import           Linnet.Aeson
import           Linnet.Output             (payloadEmpty)
import           Network.HTTP.Types.Status (status404)
import           Network.Wai               (Application)
import           Network.Wai.Handler.Warp  (run)

type IdCounter = IORef Int

type TodoStorage = IORef (Map.Map Int Todo)

app :: IdCounter -> TodoStorage -> Application
app idCounter todoStorage =
  bootstrap
    @ApplicationJson
    (postTodo idCounter todoStorage |+| patchTodo todoStorage |+| deleteTodo todoStorage |+| getTodos todoStorage) &
  compile &
  toApp id

instance Encode ApplicationJson SomeException where
  encode _ = Prelude.mempty

todos = p' "todos"

postTodo :: IdCounter -> TodoStorage -> Endpoint IO Todo
postTodo idCounter todoStorage =
  post (todos // jsonBody @(Int -> Bool -> Todo)) ~>>
  (\getTodo -> do
     id <- atomicModifyIORef idCounter (\id -> (id + 1, id + 1))
     let todo = getTodo id False
     saved <- atomicModifyIORef todoStorage (\map -> (Map.insert id todo map, todo))
     return $ created saved)

patchTodo :: TodoStorage -> Endpoint IO Todo
patchTodo todoStorage =
  patch (todos // path @Int // jsonBody @(Todo -> Todo)) ~>>
  (\id pt ->
     atomicModifyIORef
       todoStorage
       (\storage ->
          case Map.lookup id storage of
            Just todo ->
              let patched = pt todo
               in (Map.adjust (const patched) id storage, ok patched)
            Nothing -> (storage, payloadEmpty status404)))

deleteTodo :: TodoStorage -> Endpoint IO Todo
deleteTodo todoStorage =
  delete (todos // path @Int) ~>>
  (\id ->
     atomicModifyIORef
       todoStorage
       (\storage ->
          case Map.lookup id storage of
            Just todo -> (Map.delete id storage, ok todo)
            Nothing   -> (storage, payloadEmpty status404)))

getTodos :: TodoStorage -> Endpoint IO [Todo]
getTodos todoStorage =
  get todos ~>> do
    storage <- readIORef todoStorage
    return $ ok . sortOn (negate . todoId) . Map.elems $ storage

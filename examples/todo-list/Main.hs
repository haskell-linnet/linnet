module Main where

import           Data.IORef        (newIORef)
import           Examples.TodoList
import           Linnet            (run)

main :: IO ()
main = do
  idCounter <- newIORef 0
  todoStorage <- newIORef mempty
  run 9000 $ app idCounter todoStorage

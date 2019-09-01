module Main where

import           Data.IORef         (newIORef)
import           Examples.Streaming (app)
import           Linnet             (run)

main :: IO ()
main = do
  counter <- newIORef mempty
  run 9000 $ app counter

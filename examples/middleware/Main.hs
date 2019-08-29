module Main where

import           Examples.Middleware (app)
import           Linnet              (run)

main :: IO ()
main = run 9000 app

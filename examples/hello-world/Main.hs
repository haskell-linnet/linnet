module Main where

import           Examples.HelloWorld (app)
import           Linnet              (run)

main :: IO ()
main = run 9000 app

module Main where

import Interpreter
import System.Environment

main :: IO ()
main = do
  (expr:_) <- getArgs
  print $ eval $ readExpr expr

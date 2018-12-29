module Main where

import Interpreter
import Parser
import System.Environment
import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.State (get)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runEval' globalEnv loop
    1 -> evalPrint $ head args
    _ -> putStrLn "Program takes only 0 and 1 argument"

repl :: String -> IO ()
repl prompt = do
  expr <- readPrompt prompt
  if expr == "quit()"
    then return ()
    else evalPrint expr >> repl prompt

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalPrint :: String -> IO ()
evalPrint str = do
  val <- evaluate globalEnv $ readExpr str
  print val

loop :: Eval ()
loop = do
  str <- liftIO $ readPrompt ">>> "
  when (str == "quit()") $ throwError Exit
  if (str == "%who")
    then get >>= liftIO . print
    else do
        let expr = readExpr str
        val <- (fmap Right $ eval expr) `catchError` (return . Left)
        case val of
            Right v -> liftIO $ print v
            Left e -> liftIO $ print e

  loop

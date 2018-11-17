module Main where

import Interpreter
import Parser
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> repl ">>> "
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
evalPrint = print . evaluate globalEnv . readExpr

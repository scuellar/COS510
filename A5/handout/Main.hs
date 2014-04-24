module Main where

import Parser
import EDMinML
import EDTyping
import Translate
import TTyping
import TMinMLEval

import System.IO
import System.Environment
import System.Exit

import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ showUsage
  let fileName = head args
  contents <- readFile $ fileName
  let dprog = parser $ lexer $ contents
  case check_wellformed dprog of
    Nothing -> do
      putStrLn $ "'" ++ fileName ++ "' is malformed.  Sorry!"
      exitFailure
    Just _ ->
      return ()
  case translate dprog of
    Nothing -> do
      putStrLn "Translation failed.  Sorry!"
      exitFailure
    Just (tprog,ty) -> do
      putStrLn $ (show tprog)
      case typeOf tprog of
        Nothing -> do
          putStrLn "Translation does not typecheck.  Sorry!"
          exitFailure
        Just ty' -> do
          if ty == ty' then
            return ()
          else do
            putStr $ "Translator claims type is " ++ (show ty)
            putStrLn $ " but type checker produced " ++ (show ty') ++ "."
            exitFailure
      putStrLn $ "typed at " ++ show ty
      case eval tprog of
        Left error -> do
          putStrLn $ "Runtime error: " ++ show error
          exitFailure
        Right v -> putStrLn $ show v

        
showUsage :: IO ()
showUsage = do
  putStrLn "Usage: dminml [filename]"
  exitFailure

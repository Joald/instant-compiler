module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import BNFC.LexInstant
import BNFC.ParInstant
import BNFC.PrintInstant
import BNFC.AbsInstant
import BNFC.ErrM

import Processor
import Abs


type ParseFun a = [Token] -> Err a

type Verbosity = Int

myLLexer :: String -> [Token]
myLLexer = myLexer

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Mode -> Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile mode v p f = readFile f >>= run mode v p

run :: Mode -> Verbosity -> ParseFun Program -> String -> IO ()
run mode v p s =
  let ts = myLLexer s
   in case p ts of
        Bad _ -> do
          putStrLn "\nParse              Failed...\n"
          putStrV v "Tokens:"
          putStrV v $ show ts
          putStrLn s
          exitFailure
        Ok tree -> do
          processProgram mode tree
          exitSuccess


showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "usage: Call with one of the following argument combinations:       "
      , "  jvm file.ins         Compile Instant code to Jasmin instructions."
      , "  llvm file.ins        Compile Instant code to LLVM IR.            "
      , "  *                    Display this help message.                  "
      ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    "llvm":file:_ -> runFile LLVM 2 pProgram file
    "jvm":file:_ -> runFile JVM 2 pProgram file
    _ -> usage

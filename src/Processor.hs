module Processor (processProgram, checkIdents) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.Reader

import Abs
import BNFC.AbsInstant
import JVM (processJVM)
import LLVM (processLLVM)
type IdChk = ExceptT String (Reader (Set Ident))

checkExp :: Exp -> IdChk ()
checkExp (ExpAdd e1 e2) = do
  checkExp e1
  checkExp e2
checkExp (ExpSub e1 e2) = do
  checkExp e1
  checkExp e2
checkExp (ExpMul e1 e2) = do
  checkExp e1
  checkExp e2
checkExp (ExpDiv e1 e2) = do
  checkExp e1
  checkExp e2
checkExp (ExpLit _) = return ()
checkExp (ExpVar ident) = do
  s <- ask
  when (Set.notMember ident s) $ throwError $ "Undefined identifier " ++ show ident

checkIdents :: Program -> Either String ()
checkIdents (Prog l) = runReader (runExceptT $ go l) Set.empty
  where
    go (SAss ident expr:xs) = do
      checkExp expr
      local (Set.insert ident) $ go xs
    go (SExp expr:xs) = checkExp expr >> go xs
    go [] = return ()

processProgram :: Mode -> Program -> String
processProgram m p = either id (const $ _processProgram m p) $ checkIdents p

_processProgram :: Mode -> Program -> String
_processProgram JVM = processJVM
_processProgram LLVM = processLLVM

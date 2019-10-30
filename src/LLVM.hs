module LLVM (processLLVM) where


import BNFC.AbsInstant
import Abs
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe

type LLVMM = ReaderT (Map Ident Name) (State Integer)

type CodeGen = LLVMM [LLVMCode]

freshName :: LLVMM String
freshName = do
  old <- get
  put $ old + 1
  return $ 's' : show old

processLLVM :: Program -> IO ()
processLLVM prog = putStrLn $ bakeLLVM $ evalState (runReaderT (compileProgram prog) Map.empty) 0

bakeLLVM :: [LLVMCode] -> String
bakeLLVM = concatMap (("  "++) . (++"\n") . show) . (ILLVMPrelude:) . (++[ILLVMEpilogue])

compileProgram :: Program -> CodeGen
compileProgram (Prog stmts) = compileStmts stmts

compileStmts :: [Stmt] -> CodeGen
compileStmts (SAss ident expr:stmts) = do
  env <- ask
  (res, code) <- compileExpr expr
  if Map.member ident env
    then do
      let var = fromJust $ Map.lookup ident env
      rest <- compileStmts stmts
      return $ code ++ IAssignVar var res : rest
    else do
      var <- freshName
      rest <- local (Map.insert ident var) $ compileStmts stmts             
      return $ code ++ IAllocateVar var : IAssignVar var res : rest
compileStmts (SExp expr:stmts) = do
  (res, code) <- compileExpr expr
  rest <- compileStmts stmts
  return $ code ++ IPrint res : rest
compileStmts _ = return []

compileExpr :: Exp -> LLVMM (LLVMVal, [LLVMCode])
compileExpr (ExpLit i) = return (VConst i, [])
compileExpr (ExpVar ident) = do
  name <- freshName
  ptrName <- asks (fromJust . Map.lookup ident)
  return (VVar name, [ILoad name (VVar ptrName)])
  
compileExpr (ExpAdd e1 e2) = compileBinary IAdd e1 e2
compileExpr (ExpSub e1 e2) = compileBinary ISub e1 e2
compileExpr (ExpMul e1 e2) = compileBinary IMul e1 e2
compileExpr (ExpDiv e1 e2) = compileBinary IDiv e1 e2

type BinOp = Name -> LLVMVal -> LLVMVal -> LLVMCode 

compileBinary :: BinOp -> Exp -> Exp -> LLVMM (LLVMVal, [LLVMCode])
compileBinary op e1 e2 = do
  (res1, code1) <- compileExpr e1
  (res2, code2) <- compileExpr e2
  name <- freshName
  return (VVar name, code1 ++ code2 ++ [op name res1 res2])

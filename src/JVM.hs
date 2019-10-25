module JVM where

import BNFC.AbsInstant
import Abs
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe

type StackLen = Int
  
type JVMM a = ReaderT (Map Ident Int) (State StackLen) a

type CodeGen = JVMM [JVMCode]

jvmToString :: JVMCode -> String
jvmToString code = showJVM code ++ "\n"

processJVM :: Program -> String
processJVM prog = bakeJVM $ evalState (runReaderT (compileProgram prog) Map.empty) 0

bakeJVM :: [JVMCode] -> String
bakeJVM  = concatMap jvmToString . finalize . optimize

finalize :: [JVMCode] -> [JVMCode]
finalize l = IPrelude : l ++ [IEpilogue]

optimize :: [JVMCode] -> [JVMCode]
optimize _l = Ilimitstack (blockStackUsage l) : l
  where
    l = last _l : init _l

compileProgram :: Program -> CodeGen
compileProgram (Prog stmts) = compileStmts stmts 

compileStmts :: [Stmt] -> CodeGen
compileStmts (SAss ident expr:stmts) = do
  env <- ask
  code <- compileExp expr
  let new = Map.findWithDefault (Map.foldr max 0 env + 1) ident env
  let assgn = istore new
  rest <- (if Map.notMember ident env
            then local $ Map.insert ident new
            else id) (compileStmts stmts)
  return $ code ++ assgn : rest 
compileStmts (SExp expr:stmts) = do
  code <- compileExp expr
  rest <- compileStmts stmts
  return $ Igetstaticprint : code ++ Iinvokevirtualprint : rest
   
compileStmts [] = asks $ (:[]) . Ilimitlocals . succ . Map.size

-- Generates code that pushes one value on the stack, the result of the expression. 
compileExp :: Exp -> CodeGen
compileExp (ExpAdd e1 e2) = (++ [Iiadd]) <$> compileCommutative e1 e2
compileExp (ExpSub e1 e2) = (++ [Iisub]) <$> compileCommutative e1 e2
compileExp (ExpMul e1 e2) = (++ [Iimul]) <$> compileCommutative e1 e2
compileExp (ExpDiv e1 e2) = (++ [Iidiv]) <$> compileCommutative e1 e2
compileExp (ExpLit n) = return [iconst n]
compileExp (ExpVar ident) = (:[]) . iload <$> asks (fromJust . Map.lookup ident)

compileCommutative :: Exp -> Exp -> CodeGen
compileCommutative e1 e2 = do
 code1 <- compileExp e1
 code2 <- compileExp e2
 let (dep1, dep2) = (blockStackUsage code1, blockStackUsage code2)
 return $ if dep1 >= dep2
   then code1 ++ code2
   else code2 ++ code1

compileNonCommutative :: Exp -> Exp -> CodeGen
compileNonCommutative e1 e2 = do
 code1 <- compileExp e1
 code2 <- compileExp e2
 return $ code1 ++ code2
  
blockStackUsage :: [JVMCode] -> Int
blockStackUsage = foldr max 0 . scanl (+) 0 . map stackUsage 

stackUsage :: JVMCode -> Int
stackUsage Igetstaticprint = 1
stackUsage Iinvokevirtualprint = -2
stackUsage (Iiload _) = 1 
stackUsage Iiload_0 = 1
stackUsage Iiload_1 = 1
stackUsage Iiload_2 = 1
stackUsage Iiload_3 = 1
stackUsage Iiadd = -1
stackUsage Iisub = -1
stackUsage Iimul = -1
stackUsage Iidiv = -1
stackUsage (Iistore _) = -1
stackUsage Iistore_0 = -1
stackUsage Iistore_1 = -1
stackUsage Iistore_2 = -1
stackUsage Iistore_3 = -1
stackUsage (Ibipush _) = 1
stackUsage Iiconst_m1 = 1
stackUsage Iiconst_0 = 1
stackUsage Iiconst_1 = 1
stackUsage Iiconst_2 = 1
stackUsage Iiconst_3 = 1
stackUsage Iiconst_4 = 1
stackUsage Iiconst_5 = 1
stackUsage _ = 0

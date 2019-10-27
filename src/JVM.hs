module JVM where

import BNFC.AbsInstant
import Abs
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe

--                     max      cur
type JVMM = ReaderT (Integer, Integer) (State (Map Ident Integer))

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)
mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

type CodeGen = JVMM [JVMCode]

jvmToString :: JVMCode -> String
jvmToString code = showJVM code ++ "\n"

processJVM :: Program -> String
processJVM prog =
  let depth = maxStackUsage $ getStmts prog in
      bakeJVM depth $ evalState (runReaderT (compileProgram prog) (depth, 0)) Map.empty

bakeJVM :: Integer -> [JVMCode] -> String
bakeJVM depth = concatMap jvmToString . finalize . optimize depth

finalize :: [JVMCode] -> [JVMCode]
finalize l = IPrelude : l ++ [IEpilogue]

optimize :: Integer -> [JVMCode] -> [JVMCode]
optimize depth _l = Ilimitstack depth : l
  where
    l = last _l : init _l

compileProgram :: Program -> CodeGen
compileProgram (Prog stmts) = compileStmts stmts

compileStmts :: [Stmt] -> CodeGen
compileStmts (SAss ident expr:stmts) = do
  env <- get
  code <- compileExp expr
  let new = Map.findWithDefault (Map.foldr max 0 env + 1) ident env
  let assgn = istore new
  when (Map.notMember ident env) $ modify $ Map.insert ident new
  rest <- compileStmts stmts
  return $ code ++ assgn : rest
compileStmts (SExp expr:stmts) = do
  code <- compileExp expr
  rest <- compileStmts stmts
  let expDepth = exprStackUsage expr
  (maxSt, _) <- ask
  return $ if expDepth == maxSt && expDepth > 1
    then code ++ Igetstaticprint : Iswap : Iinvokevirtualprint : rest
    else Igetstaticprint : code ++ Iinvokevirtualprint : rest

compileStmts [] = gets $ (:[]) . Ilimitlocals . succ . fromIntegral . Map.size

appendInstr :: JVMCode -> CodeGen -> CodeGen
appendInstr i code = (++ [i]) <$> code

-- Generates code that pushes one value on the stack, the result of the expression.
compileExp :: Exp -> CodeGen
compileExp (ExpAdd e1 e2) = appendInstr Iiadd $ compileCommutative e1 e2
compileExp (ExpSub e1 e2) = appendInstr Iisub $ compileNonCommutative e1 e2
compileExp (ExpMul e1 e2) = appendInstr Iimul $ compileCommutative e1 e2
compileExp (ExpDiv e1 e2) = appendInstr Iidiv $ compileNonCommutative e1 e2
compileExp (ExpLit n) = return [iconst n]
compileExp (ExpVar ident) = (:[]) . iload <$> gets (fromJust . Map.lookup ident)

compileCommutative :: Exp -> Exp -> CodeGen
compileCommutative e1 e2 = do
 let (dep1, dep2) = (exprStackUsage e1, exprStackUsage e2)
 if dep1 >= dep2
   then compileOrderedExps e1 e2
   else compileOrderedExps e2 e1

compileOrderedExps :: Exp -> Exp -> CodeGen
compileOrderedExps e1 e2 = do
  code1 <- compileExp e1
  code2 <- local (mapSnd succ) $ compileExp e2
  return $ code1 ++ code2

compileNonCommutative :: Exp -> Exp -> CodeGen
compileNonCommutative e1 e2 = do
 (maxSt, curSt) <- ask
 let (dep1, dep2) = (exprStackUsage e1, exprStackUsage e2)
 if dep1 < dep2 && curSt + dep2 + 1 > maxSt
   then appendInstr Iswap $ compileOrderedExps e2 e1
   else compileOrderedExps e1 e2


maxStackUsage :: [Stmt] -> Integer
maxStackUsage  = foldr (max . stmtStackUsage) 0

stmtStackUsage :: Stmt -> Integer
stmtStackUsage (SAss _ expr) = exprStackUsage expr
stmtStackUsage (SExp expr) = max 2 $ exprStackUsage expr

exprStackUsage :: Exp -> Integer
exprStackUsage (ExpLit _) = 1
exprStackUsage (ExpVar _) = 1
exprStackUsage (ExpAdd e1 e2) = opStackUsage e1 e2
exprStackUsage (ExpSub e1 e2) = opStackUsage e1 e2
exprStackUsage (ExpMul e1 e2) = opStackUsage e1 e2
exprStackUsage (ExpDiv e1 e2) = opStackUsage e1 e2

opStackUsage :: Exp -> Exp -> Integer
opStackUsage e1 e2 =
  let (dep1, dep2) = (exprStackUsage e1, exprStackUsage e2) in
    let mini = min dep1 dep2 in
      let maxi = max dep1 dep2 in
        max maxi (mini + 1)

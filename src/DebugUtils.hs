
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module DebugUtils where
import System.Random
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import Data.Char
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import BNFC.AbsInstant
import BNFC.ParInstant
import BNFC.ErrM

twice :: Monad m => m a -> m (a, a)
twice action = do
  v1 <- action
  v2 <- action
  return (v1, v2)
type St = Map Ident Integer
type Rd = Set Char

type GenM m = (MonadIO m, MonadReader Rd m, MonadState St m)
type GenSmallM m = (MonadIO m, MonadReader Rd m)

validateExp :: MonadState St m => String -> m (Maybe Integer)
validateExp expr = do
    i <- doInterpretExp expr
    case i of
      Left _ -> return Nothing
      Right x -> return $ Just x

generateProg :: GenM m => Integer -> m String
generateProg 1 = snd <$> generateExp
generateProg n = do
  a <- chr <$> liftIO (randomRIO (ord 'a', ord 'z'))
  (x, e) <- generateExp
  assign (Ident [a]) x
  let line = a : " = " ++ e ++ ";\n"
  (line ++) <$> (local (Set.insert a) $ generateProg (n - 1))

generateExp :: GenM m => m (Integer, String)
generateExp = do
  e <- genExp 20
  res <- validateExp e
  case res of
    Nothing -> generateExp
    Just x -> return (x, e)
  where
    genExp 1 = generateAtom
    genExp n = do
      c <- generateAtom
      s <- generateSign
      rest <- genExp (n - 1)
      return $ c ++ ' ' : s : ' ' : rest

generateSign :: MonadIO m => m Char
generateSign = liftIO $ (['+', '-', '*', '/'] !!) <$> randomRIO (0, 3)

generateAtom :: GenSmallM m => m String
generateAtom = do
  set <- ask
  res <- liftIO $ randomRIO (0, 20 + Set.size set)
  return $ if res > 20
    then Set.elemAt (res - 21) set : ""
    else show res

forceParse :: String -> Program
forceParse s = let Ok boomer = pProgram $ myLexer s in boomer

type InterpM m = (MonadError String m, MonadState St m)

assign :: MonadState St m => Ident -> Integer -> m ()
assign ident val = modify $ Map.insert ident val

interpret :: InterpM m => Program -> m Integer
interpret (Prog stmts) = intStmts stmts

intStmts :: InterpM m => [Stmt] -> m Integer
intStmts [SExp expr] = interpretExpr expr
intStmts (SAss ident expr:stmts) = do
  x <- interpretExpr expr
  assign ident x
  intStmts stmts
intStmts _ = throwError "This cannot happen."


jvmMaxInt :: Integer
jvmMaxInt = 2147483647
jvmMinInt :: Integer
jvmMinInt = -2147483648

interpretExpr :: InterpM m => Exp -> m Integer
interpretExpr expr = do
  res <- intExpr expr
  when (res < jvmMinInt || jvmMaxInt < res) $ throwError "Integer overflow."
  return res

intExpr :: InterpM m => Exp -> m Integer
intExpr (ExpLit lit) = return lit
intExpr (ExpVar name) = gets $ fromJust . Map.lookup name
intExpr (ExpAdd e1 e2) = liftM2 (+) (interpretExpr e1) (interpretExpr e2)
intExpr (ExpSub e1 e2) = liftM2 (-) (interpretExpr e1) (interpretExpr e2)
intExpr (ExpMul e1 e2) = liftM2 (*) (interpretExpr e1) (interpretExpr e2)
intExpr (ExpDiv e1 e2) = do
  x1 <- interpretExpr e1
  x2 <- interpretExpr e2
  if x2 == 0
    then throwError "Div by zero"
    else return $ if x1 < 0 && x2 >= 0 || x1 >= 0 && x2 < 0
      then -(abs x1 `div` abs x2)
      else x1 `div` x2

doInterpret :: String -> Either String Integer
doInterpret s = evalState (runExceptT $ interpret $ forceParse s) Map.empty

forceParseExp :: String -> Exp
forceParseExp s = (\(Prog [SExp expr]) -> expr) (forceParse s)

doInterpretExp :: MonadState St m => String -> m (Either String Integer)
doInterpretExp = runExceptT . interpretExpr . forceParseExp

doGenerateProg :: MonadIO m => Integer -> m String
doGenerateProg size = runReaderT (evalStateT (generateProg size) Map.empty) Set.empty


-- checkDepth :: String -> IO Integer
-- checkDepth s = do
--   p <- readFile s
--   let ts = myLLexer p
--   let t = (\(Ok tr) -> tr) $ pProgram ts
--   return $ maxStackUsage $ getStmts t
--
parse :: FilePath -> IO Program
parse s = do
  p <- readFile s
  let ts = myLexer p
  return ((\(Ok tr) -> tr) $ pProgram ts)

-- DELETE END
exp2 :: Exp
exp2 = ExpAdd (ExpMul (ExpVar (Ident "a")) (ExpVar (Ident "b"))) (ExpAdd (ExpMul (ExpVar (Ident "c")) (ExpVar (Ident "d"))) (ExpAdd (ExpVar (Ident "e")) (ExpAdd (ExpVar (Ident "f")) (ExpAdd (ExpVar (Ident "g")) (ExpVar (Ident "h"))))))

exp3 :: Exp
exp3 = ExpDiv (ExpAdd (ExpMul (ExpLit 2) (ExpVar (Ident "a"))) (ExpAdd (ExpDiv (ExpVar (Ident "b")) (ExpLit 2)) (ExpAdd (ExpVar (Ident "c")) (ExpAdd (ExpVar (Ident "d")) (ExpAdd (ExpVar (Ident "e")) (ExpAdd (ExpVar (Ident "f")) (ExpAdd (ExpVar (Ident "g")) (ExpAdd (ExpVar (Ident "h")) (ExpAdd (ExpVar (Ident "i")) (ExpAdd (ExpDiv (ExpVar (Ident "j")) (ExpLit 2)) (ExpAdd (ExpVar (Ident "k")) (ExpAdd (ExpVar (Ident "l")) (ExpAdd (ExpVar (Ident "m")) (ExpVar (Ident "n"))))))))))))))) (ExpLit 10)

--printE :: Exp -> String
printE :: Exp -> IO ()
printE = putStrLn . unlines . filter (not . null) . snd . p

p :: Exp -> (Int, [[Char]])
p (ExpAdd e1 e2) = doP e1 e2 '+'
p (ExpSub e1 e2) = doP e1 e2 '-'
p (ExpMul e1 e2) = doP e1 e2 '*'
p (ExpDiv e1 e2) = doP e1 e2 '/'
p (ExpVar (Ident v)) = (length v, [v ++ "\n"])
p (ExpLit x) = (length $ show x, [show x ++ "\n"])

spaces :: Int -> [Char]
spaces n = take n $ repeat ' '

doP :: Exp -> Exp -> Char -> (Int, [[Char]])
doP e1 e2 sgn =
  let (w1, l) = p e1 in
  let (w2, r) = p e2 in
    (w1 + w2 + 3, equalize $ (spaces w1 ++ ' ' : sgn : ' ' : spaces w2) : mergeGs l r)

mergeGs :: [[Char]] -> [[Char]] -> [[Char]]
mergeGs l r =
  let m = max (length l) (length r) in
    let ll = equalize $ l ++ (take (m - length l) $ repeat []) in
    let rr = equalize $ r ++ (take (m - length r) $ repeat []) in
      mergeG ll rr

mergeG :: [[Char]] -> [[Char]] -> [[Char]]
mergeG (x:xs) (y:ys) = (init x ++ "   " ++ y) : mergeG xs ys
mergeG x [] = x
mergeG [] y = y

equalize :: [[Char]] -> [[Char]]
equalize l =
  let m = foldr (max . length) 0 l in
    map (\x -> x ++ spaces (m - length x)) l

v :: String -> Exp
v = ExpVar . Ident

subexps :: Exp -> (Exp, Exp)
subexps (ExpAdd e1 e2) = (e1, e2)
subexps (ExpSub e1 e2) = (e1, e2)
subexps (ExpMul e1 e2) = (e1, e2)
subexps (ExpDiv e1 e2) = (e1, e2)

e :: Exp
e = forceParseExp "1 - 16 * v + z - o - 16 / 16 / z * z * n - 14 * v - 10 * z / 12 - 4 * 17 + 15 / 5 + 1"

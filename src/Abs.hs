module Abs where

data JVMCode
  = IPrelude String
  | IEpilogue
  | Ilimitlocals Integer
  | Ilimitstack Integer
  | Igetstaticprint
  | Iinvokevirtualprint
  | Iiload Integer
  | Iiload_0
  | Iiload_1
  | Iiload_2
  | Iiload_3
  | Iiadd
  | Iisub
  | Iimul
  | Iidiv
  | Iistore Integer
  | Iistore_0
  | Iistore_1
  | Iistore_2
  | Iistore_3
  | Ibipush Integer
  | Iiconst_m1
  | Iiconst_0
  | Iiconst_1
  | Iiconst_2
  | Iiconst_3
  | Iiconst_4
  | Iiconst_5
  | Isipush Integer
  | Ildc Integer
  | Iswap
  -- TBC
  deriving Show

showJVM :: JVMCode -> String
showJVM (IPrelude name) = ".bytecode 47.0\n\n\
                   \.class " ++ name ++ "\n\
                   \.super java/lang/Object\n\n\
                   \.method public <init>()V\n\
                   \  aload_0\n\
                   \  invokespecial java/lang/Object/<init>()V\n\
                   \  return\n\
                   \.end method\n\n\
                   \.method public static main([Ljava/lang/String;)V"
showJVM IEpilogue = "return\n\
                    \.end method"
showJVM (Ilimitlocals x) = ".limit locals " ++ show x
showJVM (Ilimitstack x) = ".limit stack " ++ show x
showJVM Igetstaticprint = "getstatic  java/lang/System/out Ljava/io/PrintStream;"
showJVM Iinvokevirtualprint = "invokevirtual  java/io/PrintStream/println(I)V"
showJVM i = drop 1 $ show i

iload :: Integer -> JVMCode
iload 0 = Iiload_0
iload 1 = Iiload_1
iload 2 = Iiload_2
iload 3 = Iiload_3
iload n = Iiload n

istore :: Integer -> JVMCode
istore 0 = Iistore_0
istore 1 = Iistore_1
istore 2 = Iistore_2
istore 3 = Iistore_3
istore n = Iistore n

iconst :: Integer -> JVMCode
iconst 0 = Iiconst_0
iconst 1 = Iiconst_1
iconst 2 = Iiconst_2
iconst 3 = Iiconst_3
iconst 4 = Iiconst_4
iconst 5 = Iiconst_5
iconst n
  | n < 128 = Ibipush n
  | n < 32768 = Isipush n
  | otherwise = Ildc n

type Name = String

data LLVMVal
  = VConst Integer
  | VVar Name

data LLVMCode
  = IAllocateVar Name
  | IAssignVar Name LLVMVal
  | ILoad Name LLVMVal
  | IAdd Name LLVMVal LLVMVal
  | ISub Name LLVMVal LLVMVal
  | IMul Name LLVMVal LLVMVal
  | IDiv Name LLVMVal LLVMVal
  | IPrint LLVMVal
  | ILLVMPrelude
  | ILLVMEpilogue

registerize :: String -> String
registerize = ('%':)

instance Show LLVMVal where
  show (VConst x) = show x
  show (VVar name) = registerize name

instance Show LLVMCode where
  show (IAllocateVar name) = registerize name ++ " = alloca i32"
  show (IAssignVar name val) = "store i32 " ++ show val ++ ", i32* " ++ registerize name
  show (ILoad name val) = registerize name ++ " = load i32, i32* " ++ show val
  show (IAdd res l r) = registerize res ++ " = add i32 " ++ show l ++ ", " ++ show r
  show (ISub res l r) = registerize res ++ " = sub i32 " ++ show l ++ ", " ++ show r
  show (IMul res l r) = registerize res ++ " = mul i32 " ++ show l ++ ", " ++ show r
  show (IDiv res l r) = registerize res ++ " = sdiv i32 " ++ show l ++ ", " ++ show r
  show (IPrint v) = "call void @printInt(i32 " ++ show v ++ ")"
  show ILLVMPrelude = "\ndeclare void @printInt(i32)\ndefine i32 @main() {"
  show ILLVMEpilogue = "ret i32 0\n}"
  
data Mode = JVM | LLVM



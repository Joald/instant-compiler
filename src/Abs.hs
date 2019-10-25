module Abs where

data JVMCode
  = IPrelude
  | IEpilogue
  | Ilimitlocals Int
  | Ilimitstack Int
  | Igetstaticprint
  | Iinvokevirtualprint
  | Iiload Int
  | Iiload_0
  | Iiload_1
  | Iiload_2
  | Iiload_3
  | Iiadd
  | Iisub
  | Iimul
  | Iidiv
  | Iistore Int
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
  -- TBC
  deriving Show

showJVM :: JVMCode -> String
showJVM IPrelude = ".bytecode 47.0\n\n\
                   \.class C\n\
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

iload :: Int -> JVMCode
iload 0 = Iiload_0
iload 1 = Iiload_1
iload 2 = Iiload_2
iload 3 = Iiload_3
iload n = Iiload n

istore :: Int -> JVMCode
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
iconst n = Ibipush n

data LLVMCode = Undefined
data Code = JVMC JVMCode | LLVMC LLVMCode

data Mode = JVM | LLVM



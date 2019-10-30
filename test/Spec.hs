{-# LANGUAGE ScopedTypeVariables #-}
import System.IO
import DebugUtils


main :: IO ()
main = do
  let maxN :: Integer = 50
  putStrLn $ "\nGenerating " ++ show maxN ++ "tests..."
  doGeneration maxN

doGeneration :: (Eq t, Show t, Num t) => t -> IO ()
doGeneration maxN = write maxN
  where
    write 0 = putStrLn "Done!"
    write n = do
      let i = maxN - n
      putStrLn $ "Test case " ++ show i ++ ":\n"
      p <- doGenerateProg 10
      let Right res = doInterpret p
      f <- openFile ("examples/gen" ++ show i ++ ".ins") WriteMode
      hPutStrLn f p
      putStrLn  p
      fout <- openFile ("examples/gen" ++ show i ++ ".output") WriteMode
      hPrint fout res
      putStrLn $ "Result: " ++ show res ++ "\n"
      hClose f
      hClose fout
      write $ n - 1


module Main where

  import Parser
  
  import Control.Monad.Trans
  import System.Console.Haskeline
  
  -- infer a type for an input, either printing out the result or error
  interpret :: String -> IO ()
  interpret line = case parseExpr line of 
    Right validExpr  -> print validExpr
    Left err         -> print err                       

  test :: String -> IO ()
  test s = do 
    putStr "Test> "
    putStrLn s
    interpret s
    putStrLn "\n" 

  -- test cases 
  runTests :: IO ()
  runTests = return ()

  -- run a read-eval-print loop
  loop :: InputT IO ()
  loop = do
    input <- getInputLine "HM> "
    case input of
      Just "exit"  -> return ()
      Just "test"  -> lift runTests >> loop
      Just validIn -> lift (interpret validIn) >> loop
                          
  -- main method for the type inferer
  main :: IO ()
  main = do 
    putStrLn "Hindley-Milner Type Inferer, verion 1.0.0: https://github.com/thuytien140894/AlgorithmW"
    runInputT defaultSettings loop
module Main where

    import Parser
    
    import Control.Monad.Trans
    import System.Console.Haskeline
    
    -- | Infer a type for an input, either printing out the result or error
    interpret :: String -> IO ()
    interpret line = case parseExpr line of 
        Right validExpr -> print validExpr
        Left err        -> print err                       

    -- | Run test on a string input
    test :: String -> IO ()
    test s = do 
        putStr "Test> "
        putStrLn s
        interpret s
        putStrLn "\n" 

    -- | Run all the test cases 
    runTests :: IO ()
    runTests = return ()

    -- | Run a read-eval-print loop
    loop :: InputT IO ()
    loop = do
        input <- getInputLine "HM> "
        case input of
            Just "exit"  -> return ()
            Just "test"  -> lift runTests >> loop
            Just validIn -> lift (interpret validIn) >> loop
                            
    -- | Main method for the type inferer
    main :: IO ()
    main = do 
        putStrLn "Hindley-Milner Type Inferer, verion 1.0.0: https://github.com/thuytien140894/AlgorithmW"
        runInputT defaultSettings loop
module Main where

    import Parser
    import Prettier
    import TypeInferer
    
    import Control.Monad.Trans
    import System.Console.Haskeline
    
    -- | Infer a type for an input, either printing out the result or error
    interpret :: String -> IO ()
    interpret line = case parseExpr line of 
        Right validExpr -> case typeInfer validExpr of 
                               Right t  -> printRes t 
                               Left err -> printMsg err
        Left err        -> print err                       

    -- | Print new line.
    newLine :: InputT IO ()
    newLine = lift $ putStr "\n"

    -- | Run test on a string input
    test :: String -> IO ()
    test s = do 
        putStr "Test> "
        putStrLn s
        interpret s
        putStrLn "\n" 

    -- | Run all the test cases 
    runTests :: IO ()
    runTests = do 
        test "succ (pred 0)"
        test "true"
        test "\\x. 0"
        test "(\\x. x) succ 0"
        test "(\\x. if iszero x then 0 else succ 0) 0"
        test "if true then true else 0"
        test "(\\m. (\\x. if x then m 0 else m (succ 0))))"
        test "((\\m. (\\x. if true then x else m x)) (\\x. x)) 0"
        test "(\\x. x 0) (\\x. succ x)"
        test "(\\x. x true) (\\x. succ x)"
        test "(\\x. x) (\\x. 0)"
        test "(\\m. (\\x. x 0) m) (\\x. x)"
        test "(\\m. (\\x. x) m) (\\x. x)"
        test "(\\m. (\\x. x m) (\\x. x)) 0"
        test "(\\n. (\\m. (\\x. x) m) n) (\\x. x)"
        test "(\\n. (\\m. (\\x. n (m 0)))))"
        test "let m = \\x. x in m m"
        test "let m = (\\x. let y = x in y) in m m"
        test "let m = (\\x. let y = x in y) in (m m) succ 0"
        test "\\x. x x"
        test "\\m. let y = m in let x = y true in x"

    -- | Display the prompt options.
    displayMenu :: IO ()
    displayMenu = do 
        putStrLn "  Commands available from the prompt:"
        putStrLn "      test: run test cases for the GTLC"
        putStrLn "      exit: exit the program"
        putStrLn "      help: display this menu"

    -- | Run a read-eval-print loop
    loop :: InputT IO ()
    loop = do
        input <- getInputLine "HM> "
        case input of
            Just "exit"  -> return ()
            Just "test"  -> lift runTests >> loop
            Just "help"  -> lift displayMenu >> loop
            Just validIn -> lift (interpret validIn) >> newLine >> loop
                            
    -- | Main method for the type inferer
    main :: IO ()
    main = do 
        putStrLn "Hindley-Milner Type Inferer, verion 1.0.0: https://github.com/thuytien140894/AlgorithmW"
        putStrLn "Type \"help\" for more information."
        runInputT defaultSettings loop
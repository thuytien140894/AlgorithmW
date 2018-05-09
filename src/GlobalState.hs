module GlobalState where 

    import Syntax 
    import Type

    import Control.Monad.Except
    import Control.Monad.State 

    -- | A global state containing and 
    -- a current type variable name.
    data GlobalEnv = GlobalEnv 
        { varName :: Int  -- ^ New variable name
        } deriving (Eq, Show)

    -- | Monad to maintain a global state and handle errors.
    type GlobalState a = ExceptT String (State GlobalEnv) a

    -- | Create a global state for type inference. 
    runTyInfer' :: GlobalState a -> GlobalEnv -> Either String a
    runTyInfer' g = evalState (runExceptT g) 

    -- | Unwrap a GlobalState monad to extract either 
    -- an error or a final result. This method is 
    -- used for testing.
    unwrap :: GlobalState a -> Either String a 
    unwrap g = evalState (runExceptT g) initialState 
      where 
        initialState = GlobalEnv { varName = 0 }

    -- | Create a global state for type inference. 
    runTyInfer :: GlobalState a -> Either String a
    runTyInfer g = evalState (runExceptT g) initialState 
      where 
        initialState = GlobalEnv { varName = 0 }

    -- | Generate a new type variable.
    newTVar :: GlobalState Type 
    newTVar = do
        env <- get 
        let n = varName env
        put env { varName = n + 1 }
        return $ TVar n

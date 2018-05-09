module GlobalState where 

    import Error
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
    type GlobalState a = ExceptT Error (State GlobalEnv) a

    -- | Create a global state for type inference. 
    runTyInfer' :: GlobalState a -> GlobalEnv -> Either Error a
    runTyInfer' g = evalState (runExceptT g) 

    -- | Unwrap a GlobalState monad to extract either 
    -- an error or a final result. This method is 
    -- used for testing.
    unwrap :: GlobalState a -> Either Error a 
    unwrap g = evalState (runExceptT g) initialState 
      where 
        initialState = GlobalEnv { varName = 0 }

    -- | Create a global state for type inference. 
    runTyInfer :: GlobalState a -> Either Error a
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

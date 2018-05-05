module GlobalState where 

    import Syntax 
    import Type
    import TypeEnv

    import Control.Monad.Except
    import Control.Monad.State 

    -- | A global state containing a typing environment and 
    -- a current type variable name.
    data GlobalEnv = GlobalEnv 
        { typeEnv :: TypeEnv  -- ^ Typing environment
        , varName :: Int      -- ^ New variable name
        } deriving (Eq, Show)

    -- | Monad to maintain a global state and handle errors.
    type GlobalState a = ExceptT String (State GlobalEnv) a

    -- | Create a global state for type inference. 
    runTyInfer :: GlobalState a -> Either String a
    runTyInfer g = evalState (runExceptT g) initialState 
      where 
        initialState = GlobalEnv 
            { typeEnv = emptyTypeEnv
            , varName = 0
            }

    -- | Generate a new type variable.
    newTVar :: GlobalState Type 
    newTVar = do
        currentState <- get 
        let n = varName currentState
        put currentState { varName = n + 1 }
        return $ TVar n

    -- | Update an entry in the type environment.
    updateTypeEnv :: String -> TypeScheme -> GlobalState () 
    updateTypeEnv x t = do 
        s <- get 
        let newEnv = insertVar (typeEnv s) x t
        put s { typeEnv = newEnv }
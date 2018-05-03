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
    newVar :: GlobalState Type 
    newVar = do
        currentState <- get 
        let n = varName currentState
        put currentState { varName = n + 1 }
        return $ TVar n

    -- | Update an entry in the type environment.
    updateTypeEnv :: Expr -> GlobalState () 
    updateTypeEnv x = do 
        currentState <- get 
        let n = varName currentState 
        let newEnv = typeEnv currentState `insertVar` x $ TVar n
        put currentState { typeEnv = newEnv, varName = n + 1 }
module GlobalState where 

    import Syntax 
    import Type
    import TypeEnv (TypeEnv)
    
    import qualified TypeEnv

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
            { typeEnv = TypeEnv.empty
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
    update :: String -> TypeScheme -> GlobalState () 
    update x c = do 
        s <- get 
        let newEnv = TypeEnv.insert (typeEnv s) x c
        put s { typeEnv = newEnv }

    -- | Search for a variable in the type environment.
    lookUp :: String -> GlobalState TypeScheme
    lookUp x = do 
        s <- get 
        case TypeEnv.lookUp (typeEnv s) x of 
            Just c  -> return c 
            Nothing -> throwError "Variable not in scope."
module TypeEnv where 
    
    import Error
    import GlobalState (GlobalState)
    import Type

    import Control.Monad.Except (throwError)
    import Data.Map (Map)
    
    import qualified Data.Map as Map

    -- | Mapping from variables to type schemes.
    data TypeEnv = TypeEnv (Map String TypeScheme)
                   deriving (Eq, Show)

    -- | Find all free type variables in a type environment.
    instance TypeVar TypeEnv where
        freeVars (TypeEnv r) = Map.foldl (++) [] varsMap
          where 
            varsMap = Map.map freeVars r 

    -- | Empty type environment.
    empty :: TypeEnv
    empty = TypeEnv Map.empty

    -- | Insert a new variable into the type environment.
    insert :: TypeEnv -> String -> TypeScheme -> TypeEnv
    insert (TypeEnv r) x t = TypeEnv $ Map.insert x t r

    -- | Search for a variable. 
    lookUp :: TypeEnv -> String -> GlobalState TypeScheme
    lookUp (TypeEnv r) x = case Map.lookup x r of 
        Just c  -> return c 
        Nothing -> throwError $ NotBound x
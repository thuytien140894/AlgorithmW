module TypeEnv where 

    import Error
    import GlobalState (GlobalState)
    import Syntax
    import Type
    import TypeScheme (TypeScheme)

    import qualified TypeScheme

    import Control.Monad.Except (throwError)
    import Data.Map (Map)
    
    import qualified Data.Map as Map

    -- | Mapping from variables to type schemes.
    data TypeEnv = TypeEnv (Map String TypeScheme)
                   deriving (Eq, Show)

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

    -- | Find all type variables in a type environment.
    freeVars :: TypeEnv -> [Type]
    freeVars (TypeEnv r) = Map.foldl (++) [] varsMap
      where 
        varsMap = TypeScheme.freeVars `Map.map` r 
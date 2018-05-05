module TypeEnv where 

    import Type 
    import Syntax

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
    lookUp :: TypeEnv -> String -> Maybe TypeScheme
    lookUp (TypeEnv r) x = Map.lookup x r
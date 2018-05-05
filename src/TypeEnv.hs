module TypeEnv where 

    import Type 
    import Syntax

    import Data.Map (Map)
    import qualified Data.Map as Map

    -- | Mapping from variables to type schemes.
    data TypeEnv = TypeEnv (Map String TypeScheme)
                   deriving (Eq, Show)

    -- | Empty type environment.
    emptyTypeEnv :: TypeEnv
    emptyTypeEnv = TypeEnv Map.empty

    -- | Insert a new variable into the type environment.
    insertVar :: TypeEnv -> String -> TypeScheme -> TypeEnv
    insertVar (TypeEnv r) x t = TypeEnv $ Map.insert x t r
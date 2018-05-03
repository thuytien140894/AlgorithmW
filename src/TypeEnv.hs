module TypeEnv where 

    import Type 
    import Syntax

    import Data.Map (Map)
    import qualified Data.Map as Map

    data TypeEnv = TypeEnv (Map String Type)
                   deriving (Eq, Show)

    emptyTypeEnv :: TypeEnv
    emptyTypeEnv = TypeEnv Map.empty

    insertVar :: TypeEnv -> Expr -> Type -> TypeEnv
    insertVar (TypeEnv env) (Var x) t = TypeEnv $ Map.insert x t env
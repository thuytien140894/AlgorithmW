module Type where 

    import Data.Map (Map)
    import qualified Data.Map as Map

    type TVar = Type

    data TypeScheme = ForAll [TVar] TypeScheme
                    | Scheme Type
                      deriving (Eq, Show)

    data Type = TVar Int       -- ^ Type variable
              | Bool           -- ^ Boolean
              | Nat            -- ^ Natural numbers
              | Arr Type Type  -- ^ T->T 
                deriving (Eq, Show)
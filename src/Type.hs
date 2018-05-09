module Type where 

    import Data.Map (Map)
    
    import qualified Data.Map as Map

    type TVar = Type

    -- | Type scheme, which is either a type itself, 
    -- or a type that includes quantified type variables.
    data TypeScheme = ForAll [TVar] TypeScheme
                    | Scheme Type
                      deriving (Eq, Show)

    -- | Type system.
    data Type = TVar Int       -- ^ Type variable
              | Bool           -- ^ Boolean
              | Nat            -- ^ Natural numbers
              | Arr Type Type  -- ^ T->T 
                deriving (Eq, Show)
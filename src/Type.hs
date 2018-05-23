module Type where 

    import Data.List ((\\))

    -- | A type class to find free type variables.
    class TypeVar a where
        freeVars :: a -> [Type]

    type TVar = Type

    -- | Type scheme, which is either a type itself, 
    -- or a type that includes quantified type variables.
    data TypeScheme = ForAll [TVar] TypeScheme
                    | Scheme Type
                      deriving (Eq, Show)

    -- | Find all free type variables in a type scheme.
    instance TypeVar TypeScheme where 
        freeVars (Scheme t) = freeVars t
        freeVars (ForAll xs c) = freeVars c \\ xs

    -- | Type system.
    data Type = TVar Int       -- ^ Type variable
              | Boolean        -- ^ Boolean
              | Nat            -- ^ Natural numbers
              | Arr Type Type  -- ^ T->T 
                deriving (Eq, Show)

    -- | Find all free type variables in a type.
    instance TypeVar Type where 
        freeVars (TVar x) = [TVar x]
        freeVars Boolean = []
        freeVars Nat = []
        freeVars (Arr t1 t2) = freeVars t1 ++ freeVars t2
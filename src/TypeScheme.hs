module TypeScheme where 

    import Type (Type)
    
    import qualified Type 

    import Data.List ((\\))

    -- | An alias for type variables.
    type TVar = Type

    -- | Type scheme, which is either a type itself, 
    -- or a type that includes quantified type variables.
    data TypeScheme = ForAll [TVar] TypeScheme
                    | Scheme Type
                      deriving (Eq, Show)

    -- | Find all type variables in a type scheme.
    freeVars :: TypeScheme -> [Type]
    freeVars (Scheme t) = Type.freeVars t
    freeVars (ForAll xs c) = freeVars c \\ xs
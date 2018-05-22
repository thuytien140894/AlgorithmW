module Type where 

    -- | Type system.
    data Type = TVar Int       -- ^ Type variable
              | Bool           -- ^ Boolean
              | Nat            -- ^ Natural numbers
              | Arr Type Type  -- ^ T->T 
                deriving (Eq, Show)

    -- | Find all type variables in a type.
    freeVars :: Type -> [Type]
    freeVars (TVar x) = [TVar x]
    freeVars Bool = []
    freeVars Nat = []
    freeVars (Arr t1 t2) = freeVars t1 ++ freeVars t2
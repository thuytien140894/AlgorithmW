module Substitution where 

    import Type
    import TypeEnv
    
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Data.Maybe (fromMaybe)
    import qualified Data.Set as Set (fromList)

    -- | Mapping from type variables to types.
    data Substitution = Subs (Map Int Type)
                        deriving (Eq, Show)

    -- | Empty substitution.
    emptySubs :: Substitution
    emptySubs = Subs Map.empty

    -- | Look up the type for a type variable.
    lookUp :: Substitution -> Int -> Maybe Type
    lookUp (Subs s) x = Map.lookup x s

    -- | Apply a substitution to the typing environment.
    subsTEnv :: Substitution -> TypeEnv -> TypeEnv 
    subsTEnv s (TypeEnv r) = TypeEnv $ Map.map (subsTScheme s) r

    -- | Replace free type variables in a type scheme.
    subsTScheme :: Substitution -> TypeScheme -> TypeScheme
    subsTScheme s (Scheme t) = Scheme $ subsTVar s t
    subsTScheme s (ForAll xs c) = ForAll xs $ removeBoundVars s xs `subsTScheme` c

    -- | Apply a substitution to a type with type variables.
    subsTVar :: Substitution -> Type -> Type 
    subsTVar s t = case t of 
        TVar x    -> fromMaybe t $ lookUp s x
        Arr t1 t2 -> Arr (subsTVar s t1) (subsTVar s t2)
        _         -> t 

    -- | Remove all the quantified variables in a type scheme 
    -- from a substitution.
    removeBoundVars :: Substitution -> [TVar] -> Substitution
    removeBoundVars (Subs s) xs = Subs $ Map.withoutKeys s xs'
      where 
        xs' = Set.fromList $ map (\(TVar x) -> x) xs

    -- | Compose two substitutions.
    compose :: Substitution -> Substitution -> Substitution
    -- | If one substitution is empty, return the other one.
    compose s1 (Subs s2) 
        | Map.null s2 = s1
    compose (Subs s1) s2 
        | Map.null s1 = s2
    compose (Subs s1) (Subs s2) = 
        -- | Combine the rightmost substitution with the new 
        -- substitution. Prefer the new one when duplicate 
        -- type variables are found. 
        Subs $ Map.union s1'' s2  
      where 
        -- | Apply the rightmost substitution to the leftmost.
        s1'  = subsTVar (Subs s2) `Map.map` s1
        -- | Remove any mapping between two identical type 
        -- variables.
        s1'' = Map.filterWithKey mirror s1'
        mirror k a = case a of 
            TVar x 
                | k == x -> False
            _            -> True     
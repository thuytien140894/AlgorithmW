module Substitution where 

    import GlobalState 
    import Type
    import TypeEnv (TypeEnv(TypeEnv))
    
    import qualified GlobalState as GlobalS (newTVar)

    import Data.Map (Map)

    import qualified Data.Map as Map
    import qualified Data.Set as Set (fromList)

    -- | Mapping from type variables to types.
    data Substitution = Subs (Map Int Type)
                        deriving (Eq, Show)

    -- | Type class to perform substitution.
    class Substitutable a where 
        subs :: Substitution -> a -> a

    -- | Apply a substitution to a type with type variables.
    instance Substitutable Type where
        subs s (TVar x)    = case lookUp s x of 
            -- If a type variable is mapped to another one, 
            -- keep applying substitution until the resulting 
            -- type is not mapped to anything.
            Just t  -> subs s t 
            Nothing -> TVar x
        subs s (Arr t1 t2) = subs s t1 `Arr` subs s t2
        subs s t           = t

    -- | Apply a substitution to a type scheme. 
    instance Substitutable TypeScheme where 
        subs s (Scheme t)    = Scheme $ subs s t
        subs s (ForAll xs c) = ForAll xs c'
          where 
            -- | Only substitute free variables, and so 
            -- remove all quantified variables in the type 
            -- scheme from s.
            c' = removeBoundVars s xs `subs` c

    -- | Apply a substitution to the typing environment. 
    instance Substitutable TypeEnv where 
        subs s (TypeEnv r) = TypeEnv $ subs s `Map.map` r

    -- | Empty substitution.
    empty :: Substitution
    empty = Subs Map.empty

    -- | Build a new substitution by mapping a list of 
    -- type variables to new ones.
    new :: [TVar] -> Substitution -> GlobalState Substitution
    new [] s = return s
    new (TVar x:xs) s = do 
        t <- GlobalS.newTVar
        let s' = insert s x t
        new xs s'

    -- | Insert a new type variable/type pair.
    insert :: Substitution -> Int -> Type -> Substitution
    insert (Subs s) x t = Subs $ Map.insert x t s

    -- | Look up the type for a type variable.
    lookUp :: Substitution -> Int -> Maybe Type
    lookUp (Subs s) x = Map.lookup x s

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
        s1'  = subs (Subs s2) `Map.map` s1
        -- | Remove any mapping between two identical type 
        -- variables.
        s1'' = Map.filterWithKey mirror s1'
        mirror k a = case a of 
            TVar x 
                | k == x -> False
            _            -> True 
            
    -- | Compose a list of substitutions.
    composeList :: [Substitution] -> Substitution
    composeList = foldr compose empty
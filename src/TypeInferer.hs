module TypeInferer where

    import GlobalState (GlobalState) 
    import Substitution (Substitution)
    import Syntax
    import Type
    import TypeEnv (TypeEnv)

    import qualified GlobalState as GlobalS
    import qualified Substitution as Subs
    import qualified TypeEnv 

    -- | Instantiate a type scheme by 
    -- replacing all the quantified type variables 
    -- with new type variables.
    instantiate :: TypeScheme -> GlobalState Type 
    instantiate (Scheme t) = return t
    instantiate (ForAll xs c) = do 
        s <- Subs.new xs Subs.empty
        let c' = Subs.subsTScheme s c
        instantiate c' 

    -- | Return a substitution that unifies two types.
    unify :: Type -> Type -> GlobalState Substitution
    unify s t = return Subs.empty

    -- | Infer the type for an expression given a typing environment. 
    -- Also derive a substitution to be used for next type inference.
    typeInfer' :: TypeEnv -> Expr -> GlobalState (Substitution, Type)
    typeInfer' r e = case e of 
        Tru         -> return (Subs.empty, Bool)
        Fls         -> return (Subs.empty, Bool)
        Zero        -> return (Subs.empty, Nat)
        Var x       -> do c <- TypeEnv.lookUp r x
                          t <- instantiate c
                          return (Subs.empty, t)
        Lambda x e' -> do t <- GlobalS.newTVar
                          let r' = TypeEnv.insert r x $ Scheme t
                          (s', t') <- typeInfer' r' e'
                          return (s', Subs.subsTVar s' t `Arr` t') 
        App e1 e2   -> do (s1, t1) <- typeInfer' r e1
                          let r' = Subs.subsTEnv s1 r
                          (s2, t2) <- typeInfer' r' e2
                          t <- GlobalS.newTVar
                          v <- unify (Subs.subsTVar s2 t1) (Arr t2 t)
                          let s = Subs.compose v $ Subs.compose s2 s1
                          return (s, Subs.subsTVar v t)

    -- | Infer the type for an expression.
    typeInfer :: Expr -> Either String Type
    typeInfer e = case GlobalS.runTyInfer $ typeInfer' TypeEnv.empty e of 
        Right (s, t) -> Right t 
        Left err     -> Left err 
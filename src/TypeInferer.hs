module TypeInferer where

    import Error
    import GlobalState (GlobalState) 
    import Substitution (Substitution)
    import Syntax
    import Type
    import TypeEnv (TypeEnv)

    import qualified GlobalState as GlobalS
    import qualified Substitution as Subs
    import qualified TypeEnv 

    import Control.Monad.Except (throwError)

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
    unify t1 t2 
        | t1 == t2                = return Subs.empty
    unify (TVar x) t2
        | occurCheck x t2         = throwError $ Occur x t2
        | otherwise               = return $ Subs.insert Subs.empty x t2
    unify t1 (TVar x) 
        | occurCheck x t1         = throwError $ Occur x t1
        | otherwise               = return $ Subs.insert Subs.empty x t1
    unify (Arr s1 t1) (Arr s2 t2) = do s'  <- unify s1 s2 
                                       s'' <- unify (Subs.subsTVar s' t1) (Subs.subsTVar s' t2)
                                       return $ Subs.compose s'' s'
    unify t1 t2                   = throwError $ Mismatch t1 t2

    -- | Check if a type variable occurs in a given type.
    occurCheck :: Int -> Type -> Bool
    occurCheck x t = case t of
        TVar y 
            | y == x -> True 
        Arr s t      -> occurCheck x s || occurCheck x t
        _            -> False

    -- | Helper function to infer types for arithmetic expressions.
    typeInferArith :: TypeEnv -> Expr -> Type -> GlobalState (Substitution, Type)
    typeInferArith r e retTy = do 
        (s1, t1) <- typeInfer' r e
        t <- GlobalS.newTVar
        s2 <- unify (Arr Nat retTy) (Arr t1 t)
        let s = Subs.compose s2 s1 
        return (s, Subs.subsTVar s2 t)

    -- | Infer the type for an expression given a typing environment. 
    -- Also derive a substitution to be used for next type inference.
    typeInfer' :: TypeEnv -> Expr -> GlobalState (Substitution, Type)
    typeInfer' r e = case e of 
        -- | Constants
        Tru         -> return (Subs.empty, Bool)
        Fls         -> return (Subs.empty, Bool)
        Zero        -> return (Subs.empty, Nat)

        -- | Arithmetic
        Succ e'     -> typeInferArith r e' Nat
        Pred e'     -> typeInferArith r e' Nat
        IsZero e'   -> typeInferArith r e' Bool

        -- | Conditional
        If e1 e2 e3 -> do (s1, t1) <- typeInfer' r e1
                          s1' <- unify Bool t1
                          let s1'' = Subs.compose s1 s1'
                          let r1 = Subs.subsTEnv s1'' r
                          (s2, t2) <- typeInfer' r1 e2
                          let r2 = Subs.subsTEnv s2 r1 
                          (s3, t3) <- typeInfer' r2 e3
                          s4 <- unify t2 t3
                          let s' = Subs.composeList [s4, s3, s2, s1'']  
                          return (s', Subs.subsTVar s4 t2)

        -- | Variable
        Var x       -> do c <- TypeEnv.lookUp r x
                          t <- instantiate c
                          return (Subs.empty, t)

        -- | Abstraction
        Lambda x e' -> do t <- GlobalS.newTVar
                          let r' = TypeEnv.insert r x $ Scheme t
                          (s', t') <- typeInfer' r' e'
                          return (s', Subs.subsTVar s' t `Arr` t') 

        -- | Application
        App e1 e2   -> do (s1, t1) <- typeInfer' r e1
                          let r' = Subs.subsTEnv s1 r
                          (s2, t2) <- typeInfer' r' e2
                          t <- GlobalS.newTVar
                          s3 <- unify (Subs.subsTVar s2 t1) (Arr t2 t)
                          let s = Subs.composeList [s3, s2, s1]
                          return (s, Subs.subsTVar s3 t)

    -- | Infer the type for an expression.
    typeInfer :: Expr -> Either Error Type
    typeInfer e = case GlobalS.runTyInfer $ typeInfer' TypeEnv.empty e of 
        Right (s, t) -> Right t 
        Left err     -> Left err 
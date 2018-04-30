module ParseUtils where 

    import Syntax 
    import Type 

    import Data.List (elemIndex)
    import Data.Maybe

    -- | Correct the bruijn index for a bound variable.
    -- This function is called when parsing a lambda Expr 
    fixBinding :: Expr -> String -> Int -> Expr
    fixBinding t x b = case t of
        Var _ id 
            | id == x -> Var b id
        Succ t'       -> Succ $ fixBinding t' x b
        Pred t'       -> Pred $ fixBinding t' x b
        IsZero t'     -> IsZero $ fixBinding t' x b
        If t1 t2 t3   -> let t1' = fixBinding t1 x b
                             t2' = fixBinding t2 x b
                             t3' = fixBinding t3 x b
                         in If t1' t2' t3'
        Lambda t' ctx -> Lambda (fixBinding t' x $ b + 1) ctx
        App t1 t2     -> fixBinding t1 x b `App` fixBinding t2 x b
        _             -> t

    -- | Fix Bruijn indices for free variables
    fixFreeBinding :: Expr -> [String] -> [String] -> Expr
    fixFreeBinding t freeVars boundVars = case t of
        Var _ id 
            | id `elem` freeVars -> Var (getBruijnIndex id freeVars boundVars) id
        Succ t'                  -> Succ $ fixFreeBinding t' freeVars boundVars
        Pred t'                  -> Pred $ fixFreeBinding t' freeVars boundVars
        IsZero t'                -> IsZero $ fixFreeBinding t' freeVars boundVars
        If t1 t2 t3              -> let t1' = fixFreeBinding t1 freeVars boundVars
                                        t2' = fixFreeBinding t2 freeVars boundVars
                                        t3' = fixFreeBinding t3 freeVars boundVars
                                    in If t1' t2' t3'
        Lambda t' ctx            -> Lambda (fixFreeBinding t' freeVars boundVars) ctx
        App t1 t2                -> fixFreeBinding t1 freeVars boundVars `App` fixFreeBinding t2 freeVars boundVars
        _                        -> t

    -- | Get the bruijn index for a free variable
    getBruijnIndex :: String -> [String] -> [String] -> Int
    getBruijnIndex id freeVars boundVars = fromJust freeIndex + length boundVars
      where 
        freeIndex  = elemIndex id $ reverse freeVars

    -- | Retrieve the binding context of an abstraction
    getBoundVar :: Expr -> [String]
    getBoundVar t = case t of
        Lambda _ ctx -> ctx
        Succ t'      -> getBoundVar t'
        Pred t'      -> getBoundVar t'
        IsZero t'    -> getBoundVar t'
        If t1 t2 t3  -> getBoundVar t1 ++ getBoundVar t2 ++ getBoundVar t3
        App t1 t2    -> getBoundVar t1 ++ getBoundVar t2
        _            -> []

    -- | Find free variables
    getFreeVar :: Expr -> [String] -> [String]
    getFreeVar t boundVars = case t of
        Lambda t1 _ -> getFreeVar t1 boundVars
        Succ t'     -> getFreeVar t' boundVars
        Pred t'     -> getFreeVar t' boundVars
        IsZero t'   -> getFreeVar t' boundVars
        If t1 t2 t3 -> let l1 = getFreeVar t1 boundVars
                           l2 = getFreeVar t2 boundVars
                           l3 = getFreeVar t3 boundVars
                       in l1 ++ l2 ++ l3
        App t1 t2   -> getFreeVar t1 boundVars ++ getFreeVar t2 boundVars
        Var _ id    -> if id `elem` boundVars 
                           then [] 
                           else [id]
        _           -> []  

    -- | Application
    apply :: Expr -> Expr -> Expr
    apply Unit t2 = t2
    apply t1 t2   = App t1 t2

    -- | Recursively apply terms from the left
    applyFromLeft :: [Expr] -> Expr
    applyFromLeft = foldl apply Unit
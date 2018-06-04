module Prettier 
    ( printMsg
    , printRes
    ) where 

    import Error
    import Syntax
    import Type

    import Text.PrettyPrint.ANSI.Leijen (Doc, (<>), (<+>), (<$$>))
    
    import qualified Text.PrettyPrint.ANSI.Leijen as PP

    -- | Get a letter representation for a type variable.
    getVar :: Int -> Char 
    getVar x = ['a' .. 'z'] !! x

    -- | Format an exception message.
    renderException :: String -> Doc
    renderException = PP.red . PP.text

    -- | Type class to print pretty.
    class Pretty a where 
        output :: a -> Doc 

        -- | Print error message
        printMsg :: a -> IO ()
        printMsg = PP.putDoc . output 

        -- | Print an inferred type in green.
        printRes :: a -> IO ()
        printRes = PP.putDoc . PP.green . output

    -- | Print pretty for expressions.
    instance Pretty Expr where 
        output e = case e of 
            Tru         -> PP.text "true"
            Fls         -> PP.text "false"
            Zero        -> PP.text "0"
            Var x       -> PP.text x
            If e1 e2 e3 -> PP.text "if" <+> output e1 
                           <+> PP.text "then" <+> output e2 
                           <+> PP.text "else" <+> output e3 
            Succ e'     -> PP.text "succ" <+> PP.parens (output e')
            Pred e'     -> PP.text "pred" <+> PP.parens (output e')
            IsZero e'   -> PP.text "iszero" <+> PP.parens (output e')
            Let x e1 e2 -> PP.text "let" <+> PP.text x <+> PP.equals 
                           <+> output e1 <+> PP.text "in" <+> output e2
            Lambda x e' -> PP.backslash <> PP.text x <> PP.dot 
                           <+> output e'
            App e1 e2   -> case e1 of 
                               Lambda{} -> PP.parens (output e1) <+> sndTerm
                               App _ _  -> PP.parens (output e1) <+> sndTerm
                               _        -> output e1 <+> sndTerm
                             where 
                               sndTerm = case e2 of 
                                   Lambda{} -> PP.parens $ output e2
                                   App _ _  -> PP.parens $ output e2
                                   _        -> output e2

    -- | Print pretty for types.
    instance Pretty Type where 
        output t = case t of
            TVar x  -> PP.char $ getVar x
            Boolean -> PP.text "Bool"
            Nat     -> PP.text "Nat"
            Arr s t -> PP.parens $ output s <+> PP.text "->" <+> output t

    -- | Print pretty for type inference errors.
    instance Pretty Error where
        output e = renderException "Error:" <+> case e of 
            NotBound x   -> PP.squotes (PP.text x) <+> PP.text "is out of bound"
            Mismatch s t -> PP.text "Type mismatch:" 
                            <+> output s 
                            <+> PP.text "vs." 
                            <+> output t
            Occur x t    -> PP.text "Type variable" <+> PP.squotes (PP.char $ getVar x) 
                            <+> PP.text "occurs in" <+> PP.squotes (output t)
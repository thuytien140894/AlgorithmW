module Syntax where 

  import Types

  data Expr 
    = Unit
    | Tru
    | Fls
    | Zero
    | Var Int Type String
    | If Expr Expr Expr
    | Succ Expr 
    | Pred Expr 
    | IsZero Expr 
    | Lambda Type Expr [String]
    | App Expr Expr
    deriving (Eq, Show)
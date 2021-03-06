module Syntax where 

    import Type

    -- | Abstract syntax tree.
    data Expr = Unit                  -- ^ unit
              | Tru                   -- ^ true
              | Fls                   -- ^ false
              | Zero                  -- ^ 0
              | Var String            -- ^ Variable
              | If Expr Expr Expr     -- ^ If a then b else c
              | Succ Expr             -- ^ succ
              | Pred Expr             -- ^ pred
              | IsZero Expr           -- ^ iszero
              | Let String Expr Expr  -- ^ let expression.
              | Lambda String Expr    -- ^ Abstraction
              | App Expr Expr         -- ^ Application
                deriving (Eq, Show)
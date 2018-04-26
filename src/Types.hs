module Types where 

  data Type 
    = TUnit
    | Dyn
    | Bool
    | Nat
    | Arr Type Type
    deriving (Eq, Show)
module Type where 

    data Type = TUnit          -- ^ TUnit
              | Dyn            -- ^ Dynamic
              | Bool           -- ^ Boolean
              | Nat            -- ^ Natural numbers
              | Arr Type Type  -- ^ T->T 
                deriving (Eq, Show)
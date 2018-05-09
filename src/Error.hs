module Error where 

    import Type
     
    -- | Type inference errors.
    data Error = NotBound String 
               | Mismatch Type Type
               | Occur Int Type 
                 deriving (Eq, Show)
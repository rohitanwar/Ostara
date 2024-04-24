module AxiomSystem where
    
    import FOL

    data System = System [Line] deriving (Show)

    data Line = Axiom FOL | Conjecture FOL deriving (Show)

    -- systemToCNF :: System -> CNF
    -- systemToCNF (System lines) = concatMap lineToCNF lines

    


module AxiomSystem where
    
    import FOL

    data System = System [Line]

    data Line = Axiom FOL | Conjecture FOL

    systemToCNF :: System -> CNF
    systemToCNF (System lines) = concatMap lineToCNF lines


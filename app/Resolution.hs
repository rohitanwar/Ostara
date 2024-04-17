module Resolution where

import FOL 
import Cnf
import Subst

data Resolved = Resolved Clause Subst

resolvedSubst :: Resolved -> Subst
resolvedSubst (Resolved _ s) = s

resolvedClause :: Resolved -> Clause
resolvedClause (Resolved c _) = c

addTerm :: Resolved -> FOL -> Resolved
addTerm (Resolved (ORL c) s) f = Resolved (ORL (f:c)) s

addTerms :: Resolved -> [FOL] -> Resolved
addTerms r [] = r
addTerms r (t:ts) = addTerms (addTerm r t) ts

resolvable :: Clause -> Clause -> Resolved

resolvable (ORL []) (ORL []) = Resolved (ORL []) (Subst [])
resolvable (ORL []) _ = Resolved (ORL []) (Subst [])
resolvable _ (ORL []) = Resolved (ORL []) (Subst [])
resolvable (ORL (f:fs)) (ORL g)
    | not (isNull (resolvedSubst (resolvable' f (ORL g)))) = addTerms (resolvable' f (ORL g)) fs
    | not (isNull (resolvedSubst (resolvable (ORL fs) (ORL g)))) = addTerm (resolvable (ORL fs) (ORL g)) f
    | otherwise = Resolved (ORL (f:fs)) (Subst [])

resolvable' :: FOL -> Clause -> Resolved

resolvable' f (ORL []) = Resolved (ORL []) (Subst [])
resolvable' f (ORL (g:gs))
    | not (isNull (resolver f g)) = Resolved (substituteClause (resolver f g) (ORL (gs))) (resolver f g) 
    | not (isNull (resolvedSubst (resolvable' f (ORL gs)))) = addTerm (resolvable' f (ORL gs)) g
    | otherwise = Resolved (ORL (g:gs)) (Subst [])

resolver :: FOL -> FOL -> Subst

resolver (Not f) g = findSubst f g
resolver f (Not g) = findSubst f g
resolver _ _ = Subst []
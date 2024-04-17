module Resolution where

import FOL 
import Cnf
import Subst
import Data.List
import Data.Maybe

data Resolved = Resolved (Lit,Lit) Subst deriving (Show)

-- data ProofStep = ProofStep {
--     left :: Clause,
--     right :: Clause,
--     resolvent :: Clause,
--     substitution :: Subst,
--     prev :: ProofStep
-- } | Start

-- type Proof = [ProofStep]

-- makeProof :: ProofStep -> Proof
-- makeProof = reverse . makeProof'
--     where makeProof' Start = []
--           makeProof' p = p : makeProof' (prev p)

cleanCNF :: CNF -> CNF
cleanCNF clauses = nub $ mapMaybe cleanClause clauses

cleanClause :: Clause -> Maybe Clause
cleanClause literals
    | tautology = Nothing
    | otherwise = Just $ [Pos a | a <- positives] ++ [Neg a | a <- negatives]
    where
        tautology = any (\a -> negatedLiteral a `elem` literals) literals
        positives = nub $ [a | Pos a <- literals]
        negatives = nub $ [a | Neg a <- literals]

resolvedSubst :: Resolved -> Subst
resolvedSubst (Resolved _ s) = s

resolvedLit :: Resolved -> (Lit,Lit)
resolvedLit (Resolved c _) = c

resolve :: CNF -> CNF
resolve clauses =
    if changed then resolve newClauses else newClauses
    where
        newClauses = resolveCNF clauses
        changed = ((length newClauses) /= (length clauses)) || not ([] `elem` newClauses)

resolveCNF :: CNF -> CNF
resolveCNF clauses = cleanCNF (clauses ++ [a | Just a <- resolvents])
    where resolvents = [resolvePair a b | a <- clauses, b <- clauses, a /= b]

resolvePair :: Clause -> Clause -> Maybe Clause
resolvePair left right = result
    where
        substitutions = [Resolved (a,b) (findSubst a (negatedLiteral b)) | a <- left, b <- right, not (isNull (findSubst a (negatedLiteral b)))]
        result = if null substitutions then Nothing else Just . nub $ (substituteClause (subst) (delete a left ++ delete b right)) 
            where subst = resolvedSubst (head substitutions)
                  (a,b) = resolvedLit (head substitutions)
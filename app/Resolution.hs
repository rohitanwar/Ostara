module Resolution where

import FOL 
import Cnf
import Subst
import Data.List
import Data.Maybe

data Resolved = Resolved (Lit,Lit) Subst deriving (Show)

data Resolution = Resolution {
    left :: Clause,
    right :: Clause,
    resolvent :: Clause,
    substitution :: Subst
}

instance Show Resolution where
    show (Resolution l r res s) = show l ++ " | " ++ show r ++ " -> " ++ show res ++ " " ++ show s ++ "\n"

data ProofStep = ProofStep {
    step :: [Resolution],
    currentCNF :: CNF,
    parent :: ProofStep
} | ProofStart

instance Show ProofStep where
    show (ProofStart) = ""
    show (ProofStep s c p) = show s ++ "\n" ++ show p ++ "\n"

prevStep :: ProofStep -> ProofStep
prevStep ProofStart = ProofStart
prevStep (ProofStep _ _ p) = p

getCNF :: ProofStep -> CNF
getCNF ProofStart = []
getCNF (ProofStep _ c _) = c

getClause :: Resolution -> Clause
getClause (Resolution _ _ c _) = c

type Proof = [ProofStep]

makeProof :: ProofStep -> Proof
makeProof = reverse . makeProof'
    where makeProof' ProofStart = []
          makeProof' p = p : makeProof' (prevStep p)

cleanCNF :: CNF -> CNF
cleanCNF clauses = nub $ pureLitElim $ mapMaybe tautElim clauses

unitClausePref :: CNF -> CNF
unitClausePref clauses = mapMaybe (\x -> unitClausePref' (unitClauses clauses) x) clauses 

unitClausePref' :: [Lit] -> Clause -> Maybe Clause
unitClausePref' units clause = if any (\a -> ((negatedLiteral a) `elem` clause) || (a `elem` clause)) units then Just clause else Nothing

unitClauses :: CNF -> [Lit]
unitClauses clauses = [negatedLiteral x | a <- clauses, length a == 1, let x = head a]

pureLitElim :: CNF -> CNF
pureLitElim clauses = mapMaybe pureLitElim' clauses
    where pureLitElim' clause = if null (intersect clause pureLits) then Just clause else Nothing
          pureLits = pureLiterals clauses

pureLiterals :: CNF -> [Lit]
pureLiterals clauses = nub $ [a | a <- literals, not $ (negatedLiteral a `elem` literals) && (a `elem` literals)]
    where literals = nub $ concat clauses

subsumedClauseElim :: [Clause] -> [Clause]
subsumedClauseElim clauses = nub $ filter (\a -> not (any (\b -> subsumes a b) clauses)) clauses

subsumes :: Clause -> Clause -> Bool
subsumes a b = all (\x -> x `elem` b) a

tautElim :: Clause -> Maybe Clause
tautElim literals
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

resolve :: CNF -> ProofStep -> ProofStep
resolve clauses prev =
    if changed then resolve newClauses nextStep else nextStep
    where
        nextStep = resolveCNF clauses prev
        newClauses = getCNF nextStep
        changed = not (((length newClauses) == (length clauses)) || ([] `elem` newClauses))

resolveCNF :: CNF -> ProofStep -> ProofStep
resolveCNF clauses prev = ProofStep [a | Just a <- resolvents] (cleanCNF (clauses ++ [getClause a | Just a <- resolvents])) prev
    where resolvents = [resolvePair a b | a <- clauses, b <- clauses, a /= b]

resolvePair :: Clause -> Clause -> Maybe Resolution
resolvePair left right = result
    where
        substitutions = [Resolved (a,b) (findSubst a (negatedLiteral b)) | a <- left, b <- right, not (isNull (findSubst a (negatedLiteral b)))]
        result = if null substitutions then Nothing else Just $ Resolution left right (nub $ (substituteClause (subst) (delete a left ++ delete b right))) subst
            where subst = resolvedSubst (head substitutions)
                  (a,b) = resolvedLit (head substitutions)
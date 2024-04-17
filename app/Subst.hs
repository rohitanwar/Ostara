module Subst where

import FOL
import Cnf

data Subst = Subst [(Term, Term)]

instance Show Subst where
  show (Subst []) = ""
  show (Subst ((t1, t2):ts)) = (show t1) ++ " -> " ++ (show t2) ++ ", " ++ (show (Subst ts))

add :: Subst -> Subst -> Subst

add (Subst s1) (Subst s2) = Subst (s1 ++ s2)

isNull :: Subst -> Bool

isNull (Subst []) = True
isNull _ = False

isomorph :: FOL -> FOL -> Bool
isomorph (Atom p1 args1) (Atom p2 args2) = p1 == p2 && (length args1) == (length args2)
isomorph _ _ = False

findSubst :: FOL -> FOL -> Subst

findSubst (Atom p1 args1) (Atom p2 args2)
    | isomorph (Atom p1 args1) (Atom p2 args2) = findSubst' args1 args2
    | otherwise = Subst []

findSubst' :: [Term] -> [Term] -> Subst

findSubst' [x] [y]
    | isNum x && isNum y = Subst [(x, y)]
    | isNum x && isFun y && not (inFun x y) = Subst [(x, y)] 
    | isFun x && isNum y && not (inFun y x) = Subst [(y, x)]
    | isFun x && isFun y = funcSubst x y
    | otherwise = Subst []

findSubst' (x:xs) (y:ys) 
    | isNull (findSubst' [x] [y]) = Subst [] 
    | otherwise = add (findSubst' [x] [y] ) (findSubst' xs ys)

funcSubst :: Term -> Term -> Subst

funcSubst (Fun f1 args1) (Fun f2 args2)
    | f1 == f2 && (length args1) == (length args2) = findSubst' args1 args2
    | otherwise = Subst []

substitute :: Subst -> FOL -> FOL

substitute (Subst []) f = f
substitute (Subst ((t1, t2):ts)) f = substitute (Subst ts) (substitute' t1 t2 f)

substitute' :: Term -> Term -> FOL -> FOL

substitute' t1 t2 (Atom p args) = Atom p (map (\x -> if x == t1 then t2 else x) args)

substituteClause :: Subst -> Clause -> Clause
substituteClause s (ORL fs) = ORL (map (substitute s) fs)
module Resolution where

import FOL 
import Cnf
import Subst

isomorph :: FOL -> FOL -> Bool
isomorph (Atom p1 args1) (Atom p2 args2) = p1 == p2 && (length args1) == (length args2)
isomorph _ _ = False

findSubst :: FOL -> FOL -> Subst

findSubst (Atom p1 args1) (Atom p2 args2)
    | isomorph (Atom p1 args1) (Atom p2 args2) = findSubst' args1 args2
    | otherwise = Subst []

findSubst' :: [Term] -> [Terms] -> Subst

findSubsts [x] [y]
    | isNum x && isNum y = Subst [(x, y)]
    | isNum x && isFun y && not (inFun x y) = Subst [(x, y)] 
    | isFun x && isNum y && not (inFun y x) = Subst [(y, x)]
    | otherwise = Subst []

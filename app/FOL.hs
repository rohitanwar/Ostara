module FOL where

import Data.List (intersperse)
import Data.String

data Term = Num Integer
            | Fun String [Term]

data FOL = Impl FOL FOL | Iff FOL FOL |
           Atom String [Term] |
           Not FOL |
           TT | FF |
           Or FOL FOL | And FOL FOL |
           Exists (Term -> FOL) | Forall (Term -> FOL)

data Lit = Pos FOL | Neg FOL deriving (Show, Eq)

type Clause = [Lit]
type CNF = [Clause]


instance Eq Term where
  (Num n) == (Num m) = n == m
  (Fun f ts) == (Fun g us) = f == g && ts == us
  _ == _ = False

instance Eq FOL where
  (Iff p q) == (Iff r s) = p == r && q == s
  (Impl p q) == (Impl r s) = p == r && q == s
  (Atom p ts) == (Atom q us) = p == q && ts == us
  (Not p) == (Not q) = p == q
  TT == TT = True
  FF == FF = True
  (Or p q) == (Or r s) = p == r && q == s
  (And p q) == (And r s) = p == r && q == s
  (Exists p) == (Exists q) = p (Num 0) == q (Num 0)
  (Forall p) == (Forall q) = p (Num 0) == q (Num 0)
  _ == _ = False

instance Show Term where
  show (Num n) = show n
  show (Fun f []) = f
  show (Fun f ts) = f ++ "(" ++ (concat $ intersperse "," (map show ts)) ++ ")"

instance Show FOL where
  show (Iff p q) = "(" ++ (show p) ++ " ↔ " ++ (show q) ++ ")"
  show (Impl p q) = "(" ++ (show p) ++ " -> " ++ (show q) ++ ")"
  show (Atom p ts) = p ++ "(" ++ (concat $ intersperse "," (map show ts)) ++ ")"
  show (Not p) = "¬" ++ (show p)
  show TT = "⊤"
  show FF = "⊥"
  show (Or p q) = "(" ++ (show p) ++ " ∨ " ++ (show q) ++ ")"
  show (And p q) = "(" ++ (show p) ++ " ∧ " ++ (show q) ++ ")"
  show (Exists p) = "∃x(" ++ (show (p (Num 0))) ++ ")"
  show (Forall p) = "∀x(" ++ (show (p (Num 0))) ++ ")"

arity :: FOL -> Integer
arity (Atom _ ts) = fromIntegral (length ts)
arity _ = -1


isNum :: Term -> Bool
isNum (Num x) = True
isNum _ = False

isFun :: Term -> Bool
isFun (Fun x ts) = True
isFun _ = False

inFun :: Term -> Term -> Bool
inFun (Num x) (Num y) = y == x
inFun (Num x) (Fun y ts) = or (map (inFun (Num x)) ts)

negatedLiteral :: Lit -> Lit
negatedLiteral (Pos p) = Neg p
negatedLiteral (Neg p) = Pos p

replaceVarWithTerm :: FOL -> Term -> Term -> FOL
replaceVarWithTerm f v t = case f of
  Atom p ts -> Atom p (map (replaceVarWithTerm' v t) ts)
  Not p -> Not (replaceVarWithTerm p v t)
  TT -> TT
  FF -> FF
  Or p q -> Or (replaceVarWithTerm p v t) (replaceVarWithTerm q v t)
  And p q -> And (replaceVarWithTerm p v t) (replaceVarWithTerm q v t)
  Impl p q -> Impl (replaceVarWithTerm p v t) (replaceVarWithTerm q v t)
  Iff p q -> Iff (replaceVarWithTerm p v t) (replaceVarWithTerm q v t)
  Exists p -> Exists (\x -> replaceVarWithTerm (p x) v t)
  Forall p -> Forall (\x -> replaceVarWithTerm (p x) v t)

replaceVarWithTerm' :: Term -> Term -> Term -> Term
replaceVarWithTerm' v t x = if x == v then t else x
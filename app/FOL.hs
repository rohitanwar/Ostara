module FOL where

import Data.List (intersperse)
import Data.String

data Term = Num Integer
            | Fun String [Term]

data FOL = Impl FOL FOL |
           Atom String [Term] |
           Not FOL |
           TT | FF |
           Or FOL FOL | And FOL FOL |
           Exists (Term -> FOL) | Forall (Term -> FOL)

data Clause = ORL [FOL] deriving (Show)
data CNF = ANDL [Clause] deriving (Show)

instance Eq Term where
  (Num n) == (Num m) = n == m
  (Fun f ts) == (Fun g us) = f == g && ts == us
  _ == _ = False

instance Eq FOL where
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
  show (Impl p q) = "(" ++ (show p) ++ " -> " ++ (show q) ++ ")"
  show (Atom p ts) = p ++ "(" ++ (concat $ intersperse "," (map show ts)) ++ ")"
  show (Not p) = "¬" ++ (show p)
  show TT = "⊤"
  show FF = "⊥"
  show (Or p q) = "(" ++ (show p) ++ " ∨ " ++ (show q) ++ ")"
  show (And p q) = "(" ++ (show p) ++ " ∧ " ++ (show q) ++ ")"
  show (Exists p) = "∃x(" ++ (show (p (Num 0))) ++ ")"
  show (Forall p) = "∀x(" ++ (show (p (Num 0))) ++ ")"

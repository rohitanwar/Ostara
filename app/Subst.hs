module Subst where

import FOL
import Cnf

data Subst = Subst [(Term, Term)]

instance Show Subst where
  show (Subst []) = ""
  show (Subst ((t1, t2):ts)) = (show t1) ++ " -> " ++ (show t2) ++ ", " ++ (show (Subst ts))

isNull :: Subst -> Bool

isNull (Subst []) = True
isNull _ = False
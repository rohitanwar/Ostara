module Cnf where
import Data.String
import Data.List
import FOL

elimImpl :: FOL -> FOL
elimImpl (Impl p q) = Or (Not (elimImpl p)) (elimImpl q)
elimImpl (Not p) = Not (elimImpl p)
elimImpl (Or p q) = Or (elimImpl p) (elimImpl q)
elimImpl (And p q) = And (elimImpl p) (elimImpl q)
elimImpl (Exists p) = Exists (\x -> elimImpl (p x))
elimImpl (Forall p) = Forall (\x -> elimImpl (p x))
elimImpl (Atom p ts) = Atom p ts
elimImpl TT = TT
elimImpl FF = FF

shiftNot :: FOL -> FOL
shiftNot (Not (Not p)) = shiftNot p
shiftNot (Not (Or p q)) = And (shiftNot (Not p)) (shiftNot (Not q))
shiftNot (Not (And p q)) = Or (shiftNot (Not p)) (shiftNot (Not q))
shiftNot (Not (Exists p)) = Forall (\x -> shiftNot (Not (p x)))
shiftNot (Not (Forall p)) = Exists (\x -> shiftNot (Not (p x)))
shiftNot (Not (Atom p ts)) = Not (Atom p ts)
shiftNot (Not TT) = FF
shiftNot (Not FF) = TT
shiftNot (Or p q) = Or (shiftNot p) (shiftNot q)
shiftNot (And p q) = And (shiftNot p) (shiftNot q)
shiftNot (Exists p) = Exists (\x -> shiftNot (p x))
shiftNot (Forall p) = Forall (\x -> shiftNot (p x))
shiftNot (Atom p ts) = Atom p ts
shiftNot TT = TT
shiftNot FF = FF


skolem :: FOL -> [Term] -> Integer -> FOL
skolem TT vs ed = TT
skolem FF vs ed = FF
skolem (Forall p) vs ed = Forall (\x -> skolem (p x) (x:vs) ed)
skolem (And p q) vs ed = And (skolem p vs (2*ed)) (skolem q vs (2*ed+1))
skolem (Or p q) vs ed = Or (skolem p vs (2*ed)) (skolem q vs (2*ed+1))
skolem (Exists p) vs ed = skolem (p (Fun ("Skol" ++ show ed) vs)) vs (2*ed)
skolem f vs _ = f

prenex :: FOL -> Integer -> FOL
prenex TT _ = TT
prenex FF _ = FF
prenex (And p q) n = And (prenex p (2*n)) (prenex q (2*n+1))
prenex (Or p q) n = Or (prenex p (2*n)) (prenex q (2*n+1))
prenex (Forall p) n = p (Num n)
prenex (Atom s ts) n = Atom s ts
prenex (Not p) n = (Not (prenex p n))

distribute :: FOL -> FOL
distribute (Or p (And q r)) = And (distribute (Or p q)) (distribute (Or p r))
distribute (Or (And q r) p) = And (distribute (Or q p)) (distribute (Or r p))
distribute (Or p q)
  | distribute p == p && distribute q == q = Or p q
distribute (Or p q) =  distribute (Or (distribute p) (distribute q))
distribute (And p q) = And (distribute p) (distribute q)
distribute TT = TT
distribute FF = FF
distribute (Not p) = Not (distribute p)
distribute (Atom p ts) = Atom p ts


cnf :: FOL -> [Term] -> Integer -> Integer -> FOL
cnf p ts ud ed = distribute (prenex ((skolem . shiftNot . elimImpl) p ts ed ) ud)

test :: FOL -> [Term] -> Integer -> FOL
test = skolem . shiftNot . elimImpl

deconstructClause :: Clause -> [FOL]
deconstructClause (ORL fs) = fs

deconstructCNF :: CNF -> [Clause]
deconstructCNF (ANDL fs) = fs

orl :: FOL -> Clause
orl (Or p q) = ORL ( (deconstructClause (orl p) ) ++ (deconstructClause (orl q) ) )
orl p = ORL [p]

propList :: FOL -> CNF
propList (And p q) = ANDL ( (deconstructCNF (propList p) ) ++ (deconstructCNF (propList q) ) )
propList p =  ANDL [orl p]



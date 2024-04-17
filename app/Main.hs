import Data.String
import Data.List

data Term = Num Integer
            | Fun String [Term]

data FOL = Impl FOL FOL |
           Atom String [Term] |
           Not FOL |
           TT | FF |
           Or FOL FOL | And FOL FOL |
           Exists (Term -> FOL) | Forall (Term -> FOL)

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

-- TODO : Implement
--equivalent :: FOL -> FOL -> Bool

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

propList :: FOL -> [FOL]

propList (And p q) = propList p ++ propList q
propList p = [p]

main :: IO ()
main = do
  let p = Forall (\x -> Exists (\y -> Atom "P" [x, y]))
  let a = (Exists (\p -> (And (Atom "Person" [p ]) (Forall (\f -> (Impl (Atom "Food" [f ]) (Atom "Eats" [p, f ])))))))
  let b = (Not (Exists ( \f -> (And (Atom "Food" [f ]) (Not (Exists (\p -> (And (Atom "Person" [p ]) (Atom "Eats" [p,f])))))))))
  let foodFact = Impl a b
  let newFact = Exists (\z -> (And (Impl a b) (Atom "Rohit" [z]) ))
  putStrLn $ show $ cnf newFact [] 1 1
  return ()

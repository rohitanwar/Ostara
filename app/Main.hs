import Cnf
import FOL
import Subst
import Resolution

main :: IO ()
main = do
  -- let p = Forall (\x -> Exists (\y -> Atom "P" [x, y]))
  -- let a = (Exists (\p -> (And (Atom "Person" [p ]) (Forall (\f -> (Impl (Atom "Food" [f ]) (Atom "Eats" [p, f ])))))))
  -- let b = (Not (Exists ( \f -> (And (Atom "Food" [f ]) (Not (Exists (\p -> (And (Atom "Person" [p ]) (Atom "Eats" [p,f])))))))))
  -- let foodFact = Impl a b
  -- let newFact = Exists (\z -> (And (Impl a b) (Atom "Rohit" [z]) ))
  -- let axioms =  propList $cnf newFact [] 1 1
  let p1 = Not (Atom "Loves" [Num 2, Num 3])
  let p2 = Atom "Loves" [Fun "f" [Num 1], Num 1]
  let p3 = Atom "Animal" [Fun "f" [Num 1]]
  let p4 = Not (Atom "Kills" [Num 2, Num 3 ])
  let c1 = ORL [p1, p4]
  let c2 = ORL [p3, p2]
  let res = resolvedClause $ resolvable c1 c2
  putStrLn $ show res
  -- let subs = findSubst p1 p2
  -- putStrLn $ show $ substitute subs p1
  return ()

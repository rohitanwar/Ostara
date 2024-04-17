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

  let f1 = Forall (\x -> Impl (Atom ("food") [x] ) (Atom "likes" [Fun "John" [],x]))
  let f2 = And (Atom "food" [Fun "Apple" []]) (Atom "food" [Fun "Vegetable" []])
  let f3 = Forall (\x -> Forall (\y -> Impl (And (Atom "eats" [x,y]) (Not (Atom "killed" [x]))) (Atom "food" [y])) )
  let f4 = And (Atom "eats" [Fun "Anil" [], Fun "Peanuts" []]) (Atom "alive" [Fun "Anil" []])
  let f5 = Forall (\x -> Iff (Not (Atom "killed" [x])) (Atom "alive" [x]))
  let conj = Atom "likes" [Fun "John" [], Fun "Peanuts" []]
  
  
  let negatedConj = Not conj
  let fact = And (And (And f1 f3) (And f4 f5)) (And negatedConj f2) 

  let system = toCNF $ cnf fact [] 1 1
  let final = resolve system
  putStrLn $ show $ final
  putStrLn $ show $ ([] `elem` final) 

  -- let c1 = orl $ cnf f1 [] 1 1
  -- let negatedConj = Not conj
  -- let c2 = orl $ cnf negatedConj [] 1 1
  -- let r = resolvable c1 c2
  -- putStrLn $ show c1
  -- putStrLn $ show c2
  -- putStrLn $ show r

  -- let p1 = Atom "Loves" [Num 2, Num 3]
  -- let p2 = Atom "Loves" [Fun "f" [Num 1], Num 1]
  -- let p3 = Atom "Animal" [Fun "f" [Num 1]]
  -- let p4 = Not (Atom "Kills" [Num 2, Num 3 ])
  -- let c1 = ORL [p1, p4]
  -- let c2 = ORL [p3, p2]
  -- let res = resolvedClause $ resolvable c1 c2
  -- putStrLn $ show res
  -- let p1' = toLiteral p1
  -- let p2' = toLiteral p2
  -- let subs = findSubst p2' p1'
  -- putStrLn $ show subs
  -- putStrLn $ show $ substitute subs p1'
  return ()

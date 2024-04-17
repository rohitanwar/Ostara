import Cnf
import FOL
import Resolution

main :: IO ()
main = do
  let p = Forall (\x -> Exists (\y -> Atom "P" [x, y]))
  let a = (Exists (\p -> (And (Atom "Person" [p ]) (Forall (\f -> (Impl (Atom "Food" [f ]) (Atom "Eats" [p, f ])))))))
  let b = (Not (Exists ( \f -> (And (Atom "Food" [f ]) (Not (Exists (\p -> (And (Atom "Person" [p ]) (Atom "Eats" [p,f])))))))))
  let foodFact = Impl a b
  let newFact = Exists (\z -> (And (Impl a b) (Atom "Rohit" [z]) ))
  let axioms =  propList $cnf newFact [] 1 1
  putStrLn $ show axioms
  return ()

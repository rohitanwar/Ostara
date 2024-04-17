import Cnf
import FOL
import Subst
import Resolution
import Parser

import System.IO
import Control.Monad

main :: IO ()
main = do

  -- let f1 = Forall (\x -> Impl (Atom ("food") [x] ) (Atom "likes" [Fun "John" [],x]))
  -- let f2 = And (Atom "food" [Fun "Apple" []]) (Atom "food" [Fun "Vegetable" []])
  -- let f3 = Forall (\x -> Forall (\y -> Impl (And (Atom "eats" [x,y]) (Not (Atom "killed" [x]))) (Atom "food" [y])) )
  -- let f4 = And (Atom "eats" [Fun "Anil" [], Fun "Peanuts" []]) (Atom "alive" [Fun "Anil" []])
  -- let f5 = Forall (\x -> Iff (Not (Atom "killed" [x])) (Atom "alive" [x]))
  -- let conj = Atom "likes" [Fun "John" [], Fun "Peanuts" []]
  
  -- let negatedConj = Not conj
  -- let fact = And (And (And f1 f3) (And f4 f5)) (And negatedConj f2) 

  -- let system = toCNF $ cnf fact [] 1 1
  -- let final = resolve system ProofStart
  -- let proof = makeProof final

  -- putStrLn $ show $ proof
  -- putStrLn $ show $ ([] `elem` (getCNF final)) 

  let f1 = "!1 : Food(1) -> Likes(John,1)"
  let f2 = "Food(Apple) & Food(Vegetable)"
  let f3 = "!1 : !2 : Eats(1,2) & ~Killed(1) -> Food(2)"
  let f4 = "Eats(Anil,Peanuts) & Alive(Anil)"
  let f5 = "!1 : ~Killed(1) <-> Alive(1)"
  let conj = "Likes(John,Peanuts)"

  contents <- readFile "app/test.fi"
  let a1 = toCNF $ cnf (Not ( getOutput (runParser parseSentence contents))) [] 1 1

  putStrLn $ show $ a1
  
  return ()
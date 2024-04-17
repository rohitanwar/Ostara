module Parser where

import Control.Monad
import Data.Char
import FOL

-- Parser for FOL formulas

data Parsed = Maybe (CNF, String)

isSpace :: Char -> Bool
isSpace c = c `elem` " \t\n\r"

skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

-- Parser for atomic formulas
parseAtom :: String -> Maybe (FOL, String)
parseAtom input = do
  let (atom, rest) = span isAlphaNum input
  guard (not (null atom))
  return (Atom atom, rest)

-- Parser for negation
parseNot :: String -> Maybe (FOL, String)
parseNot input = do
  guard (not (null input) && head input == '~')
  (formula, rest) <- parseFormula (tail input)
  return (Not formula, rest)

-- Parser for conjunction
parseAnd :: String -> Maybe (FOL, String)
parseAnd input = do
  (left, rest1) <- parseFormula input
  guard (not (null rest1) && head rest1 == '&')
  (right, rest2) <- parseFormula (tail rest1)
  return (And left right, rest2)

-- Parser for disjunction
parseOr :: String -> Maybe (FOL, String)
parseOr input = do
  (left, rest1) <- parseFormula input
  guard (not (null rest1) && head rest1 == '|')
  (right, rest2) <- parseFormula (tail rest1)
  return (Or left right, rest2)

-- Parser for implication
parseImply :: String -> Maybe (FOL, String)
parseImply input = do
  (left, rest1) <- parseFormula input
  guard (not (null rest1) && take 2 rest1 == "->")
  (right, rest2) <- parseFormula (drop 2 rest1)
  return (Imply left right, rest2)

-- Parser for universal quantification
parseForall :: String -> Maybe (FOL, String)
parseForall input = do
  guard (not (null input) && head input == 'A')
  let (var, rest1) = span isAlphaNum (tail input)
  guard (not (null var))
  (formula, rest2) <- parseFormula rest1
  return (Forall var formula, rest2)

-- Parser for existential quantification
parseExists :: String -> Maybe (FOL, String)
parseExists input = do
  guard (not (null input) && head input == 'E')
  let (var, rest1) = span isAlphaNum (tail input)
  guard (not (null var))
  (formula, rest2) <- parseFormula rest1
  return (Exists var formula, rest2)

-- Parser for the overall FOL formula
parseFormula :: String -> Maybe (FOL, String)
parseFormula input = parseAtom input
                 <|> parseNot input
                 <|> parseAnd input
                 <|> parseOr input
                 <|> parseImply input
                 <|> parseForall input
                 <|> parseExists input

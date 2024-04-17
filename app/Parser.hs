module Parser where

  import Control.Applicative
  import FOL
  import AxiomSystem
  newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

  instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> do
      (a, s') <- p s
      Just (f a, s')
  
  instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    (Parser p1) <*> (Parser p2) = Parser $ \s -> do
      (f, s') <- p1 s
      (a, s'') <- p2 s'
      Just (f a, s'')
  
  instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \s -> do
      (a, s') <- p s
      runParser (f a) s'
  
  instance MonadFail Parser where
    fail _ = Parser $ \s -> Nothing
  
  instance Alternative Parser where
    empty = fail ""
    (Parser p1) <|> (Parser p2) = Parser $ \s -> 
      case p1 s of
        Nothing -> p2 s
        r -> r

  satisfy :: (Char -> Bool) -> Parser Char
  satisfy f = Parser $ \s -> case s of
    [] -> Nothing
    (c:cs) -> if f c then Just (c, cs) else Nothing
  
  char :: Char -> Parser Char
  char c = satisfy (== c)

  string :: String -> Parser String
  string = mapM char

  space :: Parser Char
  space = char ' ' <|> char '\n' <|> char '\t' <|> char '\r'

  whitespace = many space

  parseAlphaS :: Parser Char
  parseAlphaS = satisfy (\c -> c `elem` ['a'..'z'])

  parseAlphaC :: Parser Char
  parseAlphaC = satisfy (\c -> c `elem` ['A'..'Z'])

  parseDigit :: Parser Char
  parseDigit = satisfy (\c -> c `elem` ['0'..'9'])

  strToInt :: String -> Integer -> Integer
  strToInt [] acc = acc
  strToInt (c:cs) acc = strToInt cs (acc * 10 + (charToInt c))

  charToInt :: Char -> Integer
  charToInt '0' = 0
  charToInt '1' = 1
  charToInt '2' = 2
  charToInt '3' = 3
  charToInt '4' = 4
  charToInt '5' = 5
  charToInt '6' = 6
  charToInt '7' = 7
  charToInt '8' = 8
  charToInt '9' = 9


  parseInt :: Parser Integer
  parseInt = do
    digits <- some parseDigit
    return $ strToInt digits 0

  parseExists :: Parser Char
  parseExists = (char '∃') <|> (char '?')

  parseForall :: Parser Char
  parseForall = (char '∀') <|> (char '!')

  parseVariable :: Parser Term
  parseVariable = do
    v <- parseInt
    return $ Num v

  parseNameS :: Parser String
  parseNameS = some parseAlphaS

  parseNameC :: Parser String
  parseNameC = (:) <$> parseAlphaC <*> many (parseAlphaC <|> parseAlphaS)

  parseAnd :: Parser Char
  parseAnd = (char '∧') <|> (char '&')

  parseOr :: Parser Char
  parseOr = (char '∨') <|> (char '|')

  parseNot :: Parser Char
  parseNot = (char '¬') <|> (char '~')

  parseImp :: Parser String
  parseImp = string "->"

  parseIff :: Parser String
  parseIff = string "<->"

  parseLParen :: Parser Char
  parseLParen = char '('

  parseRParen :: Parser Char
  parseRParen = char ')'

  parseComma :: Parser Char
  parseComma = char ','

  parseColon :: Parser Char
  parseColon = char ':'

  parseDefn :: Parser String
  parseDefn = string ":="

  parseSemiColon :: Parser Char
  parseSemiColon = char ';'

  parseEOL :: Parser Char
  parseEOL = char '.'

  parseEOF :: Parser Char
  parseEOF = char '\0'

  parseComment :: Parser String
  parseComment = do
    char '#'
    many (satisfy (/= '\n'))

  parseWhitespace :: Parser String
  parseWhitespace = many space

  parseArgs' :: Parser [Term]
  parseArgs' = do
    parseLParen
    args <- sepBy (parseVariable <|> parseFunction) parseComma
    parseRParen
    return args

  parseArgs :: Parser [Term]
  parseArgs = parseArgs' <|> return []


  sepBy :: Parser a -> Parser b -> Parser [a]
  sepBy p sep = sepBy1 p sep <|> return []

  sepBy1 :: Parser a -> Parser b -> Parser [a]
  sepBy1 p sep = do
    x <- p
    xs <- many (sep >> p)
    return (x:xs)

  andThen :: Parser a -> Parser b -> Parser (a, b)
  andThen p1 p2 = do
    a <- p1
    b <- p2
    return (a, b)

  parseFunction :: Parser Term
  parseFunction = do
    name <- parseNameC
    args <- parseArgs
    return $ Fun name args

  parseAtom :: Parser FOL
  parseAtom = do
    name <- parseNameC
    args <- parseArgs
    return $ Atom name args
  

  parseAndFOL :: Parser FOL
  parseAndFOL = do
    f1 <- parseFOL
    parseWhitespace
    parseAnd
    parseWhitespace
    f2 <- parseSentence
    return $ And f1 f2

  parseOrFOL :: Parser FOL
  parseOrFOL = do
    f1 <- parseFOL
    parseWhitespace
    parseOr
    parseWhitespace
    f2 <- parseSentence
    return $ Or f1 f2

  parseImpFOL :: Parser FOL
  parseImpFOL = do
    f1 <- parseFOL
    parseWhitespace
    parseImp
    parseWhitespace
    f2 <- parseSentence
    return $ Impl f1 f2
  
  parseIffFOL :: Parser FOL
  parseIffFOL = do
    f1 <- parseFOL
    parseWhitespace
    parseIff
    parseWhitespace
    f2 <- parseSentence
    return $ Impl f1 f2
  
  parseNotFOL :: Parser FOL
  parseNotFOL = do
    parseNot
    parseWhitespace
    f <- parseSentence
    return $ Not f

  parseParenFOL :: Parser FOL
  parseParenFOL = do
    parseLParen
    f <- parseSentence
    parseRParen
    return f

  parseForallFOL :: Parser FOL
  parseForallFOL = do
    parseForall
    parseWhitespace
    v <- parseVariable
    parseWhitespace
    parseColon
    parseWhitespace
    f <- parseSentence
    return $ Forall (\x -> replaceVarWithTerm f v x)
  
  parseExistsFOL :: Parser FOL
  parseExistsFOL = do
    parseExists
    parseWhitespace
    v <- parseVariable
    parseWhitespace
    parseColon
    parseWhitespace
    f <- parseSentence
    return $ Exists (\x -> replaceVarWithTerm f v x)

  parseFOL :: Parser FOL
  parseFOL = parseParenFOL <|> parseNotFOL <|> parseAtom

  parseSentence :: Parser FOL
  parseSentence = parseForallFOL <|> parseAndFOL <|> parseOrFOL <|> parseImpFOL <|> parseIffFOL <|> parseFOL


  getOutput :: Maybe (FOL, String) -> FOL
  getOutput (Just (f, _)) = f
  
  parseLine :: Parser FOL
  parseLine = parseAxiom <|> parseConjecture

  parseAxiom :: Parser FOL
  parseAxiom = do
    parseWhitespace
    string "Axiom"
    parseWhitespace
    parseDefn
    parseWhitespace
    f <- parseSentence
    parseEOL
    return f

  parseConjecture :: Parser FOL
  parseConjecture = do
    parseWhitespace
    string "Conjecture"
    parseWhitespace
    parseDefn
    parseWhitespace
    f <- parseSentence
    parseEOL
    return f
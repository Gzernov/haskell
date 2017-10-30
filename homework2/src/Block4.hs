module Block4
  (
    abParser,
    abParser_,
    intPair,
    intOrUppercase,
    zeroOrMore,
    oneOrMore,
    satisfy,
    spaces,
    ident,
    parseSExpr,
    Ident ( .. ),
    Atom ( .. ),
    SExpr ( .. ),
    Parser ( .. ),
    parseAndSimplify
  ) where

import Control.Monad (void, join)
import Data.Char ( isDigit, isUpper, isSpace, isAlpha, isAlphaNum )
import Control.Applicative ( Alternative (empty, (<|>) ) )

--Definition and examples
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

-- Real implementation
instance Functor Parser where
  fmap f (Parser a) = Parser (\x -> sub (a x) f) -- 1

  -- LAW 1. fmap id           ≡ id
  --        fmap id Parser(p) ≡ Parser (\x -> sub (p x) id) -- 1
  --                          ≡ Parser (\x -> Nothing) , p x == Nothing -- sub 1
  --                          ≡ Parser (\x -> Just (id a, b)), p x == Just (a, b) --sub 2
  --                          ≡ Parser (p)

sub :: Maybe (a, b) -> (a -> c) -> Maybe (c, b)
sub Nothing _ = Nothing --1
sub (Just (a, b)) f = Just (f a, b) --2

instance Applicative Parser where
  pure s = Parser(\x -> Just (s, x)) --1
  Parser f <*> p = Parser (\x -> getMaybe (f x) p) --2
    where
      getMaybe :: Maybe(a -> b, String) -> Parser a -> Maybe (b, String)
      getMaybe Nothing _ = Nothing --3
      getMaybe (Just (f, a)) (Parser val) = sub (val a) f --4

      -- LAW 1. pure id <*> v ≡ v
      --        pure id <*> v ≡ Parser (\y -> Just (id, y)) <*> v --1
      --                      ≡ Parser (\x -> getMaybe ((\y -> Just (id, y)) x) v) --2
      --                      ≡ Parser (\x -> getMaybe Just (id, x) v) --eval lambda
      --                      ≡ Parser (\x -> sub (p x) id) --4, x == Parser(p)
      --                      ≡ Parser (p) --see Functor

toTuple :: a -> b -> (a, b)
toTuple a b = (a, b)

abParser :: Parser (Char, Char)
abParser = toTuple <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = (\x y -> [x, y]) <$> (const <$> posInt <*> char ' ') <*> posInt

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser a <|> Parser b = Parser (\x -> a x <|> b x)

intOrUppercase :: Parser ()
intOrUppercase = void (satisfy isUpper) <|> void posInt

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (++) <$> ((: []) <$> p) <*> zeroOrMore p

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (++) <$> ((: []) <$> satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

type Ident = String

data Atom = N Integer | I Ident
  deriving (Show, Eq)

data SExpr = A Atom | Comb [SExpr]
  deriving (Show, Eq)

parseSExpr :: Parser SExpr
parseSExpr =
  spaces *> char '(' *> (getSExpr <$> zeroOrMore parseAtom) <* spaces <* char ')'
  <|> spaces *> parseBase <* spaces
    where
      getSExpr :: [SExpr] -> SExpr
      getSExpr [h] = h
      getSExpr lst = Comb lst
      parseBase :: Parser SExpr
      parseBase =
        pure (A . N) <*> (spaces *> posInt) <|>
        (pure (A . I) <*> (spaces *> ident))
      parseAtom :: Parser SExpr
      parseAtom = parseBase <|> parseSExpr

instance Monad Parser where
  return = pure --1
  p >>= f = Parser (\x -> sub (runParser p x) f)
    where
      sub :: Maybe (a, String) -> (a -> Parser b) -> Maybe (b, String)
      sub Nothing _ = Nothing
      sub (Just (a, str)) f = runParser (f a) str

data LetSum = Num Integer | Var String
  deriving Show

data LetExpr = Let String [LetSum]
  deriving Show

parseStr :: String -> Parser String
parseStr [x] = (: []) <$> char x
parseStr (x:xs) = (++) <$> parseStr [x] <*> parseStr xs

parseLet :: Parser [LetExpr]
parseLet = zeroOrMore parseLine

parseLine :: Parser LetExpr
parseLine = Let <$> (spaces *> parseStr "let" *> spaces *>
  ident <* spaces <* char '=' <* spaces) <*> parseExpr
    where
      parseExpr :: Parser [LetSum]
      parseExpr = (:) <$> parseBase <*> zeroOrMore (spaces *> char '+' *> spaces *> parseBase)
      parseBase :: Parser LetSum
      parseBase = Var <$> ident <|> Num <$> posInt

parseAndSimplify :: String -> Maybe [String]
parseAndSimplify x = sequenceA (getLetExpr x) >>= Just . map getStr
  where
    getLetExpr :: String -> [Maybe (String, Integer)]
    getLetExpr str = foldLet $ map (:[]) $ sequenceA $ map toLetSum . fst <$> runParser parseLet str

    foldLet :: [[Maybe (String, [LetSum])]] -> [Maybe (String, Integer)]
    foldLet = foldl (\a b -> a ++ map toInt (replace a b)) []

    toLetSum :: LetExpr -> (String, [LetSum])
    toLetSum (Let y x) = (y, x)

    toInt :: Maybe (String, [LetSum]) -> Maybe (String, Integer)
    toInt Nothing = Nothing
    toInt (Just (s, lst))= sequenceA (s, sum <$> mapM toIntBase lst)

    toIntBase :: LetSum -> Maybe Integer
    toIntBase (Num n) = Just n
    toIntBase _ = Nothing

    replace :: [Maybe (String, Integer)] -> [Maybe (String, [LetSum])] -> [Maybe (String, [LetSum])]
    replace [] x = x
    replace _ [Nothing] = [Nothing]
    replace (Nothing : _) _ = [Nothing]
    replace (Just (x, xv):xs) [Just (y, yv)] = replace xs [Just (y, map (rf x xv) yv)]

    rf :: String -> Integer -> LetSum -> LetSum
    rf a b v@(Var s) =
      if s == a
      then
        Num b
      else
        v
    rf _ _ x = x

    getStr :: (String, Integer) -> String
    getStr (a, b) = "let " ++ a ++ " = " ++ show b

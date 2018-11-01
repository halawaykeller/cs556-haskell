
{-
Kimberly Keller
Oct 22, 2018
Parser Monad Exercise
-}
import Control.Monad
import Control.Monad.Plus
import Control.Applicative
import Data.Char

newtype Parser a = Parser {parse :: String -> Maybe (String, a)}

data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving Show

instance Functor Parser where
    fmap f a = Parser (\ cs -> case (parse a) cs of 
        Just (s, a) -> Just (s, f a)
        Nothing -> Nothing)

instance Applicative Parser where
    pure = return
    (<*>) = ap 

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

instance Monad Parser where
    return a = Parser (\ cs -> Just (cs, a))
    (>>=) p f = Parser (\ cs -> case (parse p) cs of 
        Just (s, b) -> parse (f b) s
        Nothing -> Nothing )

instance MonadPlus Parser where
    mzero = Parser  (\ _ -> Nothing)
    mplus p q = Parser (\ cs -> (parse p cs) `mplus` (parse q cs))

{- 
Everything that follows is heavily influenced by the
functional pearl on parsing by Hutton and Meijer.
Some of their combinators I use unchanged, I hope that's ok. 
It took me long enough as it is to figure out the rest. 
-}

item :: Parser Char
item = Parser (\ cs -> case cs of
    "" -> Nothing
    (c:cs) -> Just (cs, c))

sat :: (Char -> Bool) -> Parser Char
sat p = do 
    c <- item
    if p c
    then return c
    else empty

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String 
string "" = return ""
string (c:cs) = do 
    char c
    string cs
    return (c:cs)

btree :: String -> Maybe (String, Btree Int)
btree cs = apply btreeP $ strip cs

btreeP :: Parser (Btree Int)
btreeP = do 
    {symb "Leaf"; x <- many digit; return (Leaf $ fromDigits x)} 
    `mplus` do 
        {symb "Fork"; x <- btreeP; y <- btreeP; return (Fork x y)}

strip :: String -> String
strip xs = filter (not . (`elem` "()")) xs

apply  :: Parser a -> String -> Maybe (String,a)
apply p = parse (do {space; p})

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

digit :: Parser Int
digit  = do 
    x <- token (sat isDigit)
    return (ord x - ord '0')

space :: Parser String
space = many (sat isSpace)

token  :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

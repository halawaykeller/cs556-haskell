import Control.Monad
import Control.Monad.Plus
import Control.Applicative
import Data.Char

module Parser where

-- Parser Data Type
newtype Parser a = Parser {parse :: String -> Maybe (String, a)}


-- Parser Instances
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


-- Parser Utility Functions
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

apply  :: Parser a -> String -> Maybe (String,a)
apply p = parse (do {space; p})
   
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
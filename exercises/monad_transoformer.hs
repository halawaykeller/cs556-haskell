{-
Kimberly Keller
Monad Transformer Exercise
11/20/18
-}
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.Trans.List
import Control.Applicative.Lift
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Applicative
import Data.Char
import Data.Maybe

type Parser a = StateT String Maybe a

type Dictionary = [String]
type Parser' a = ReaderT Dictionary (StateT String Maybe) a
 
car :: [a] -> Maybe a
car (x:xs) = Just x
car [] = Nothing

cdr :: [a] -> Maybe [a]
cdr (x:xs) = Just xs
cdr [] = Nothing

cadddr ::  [a] -> Maybe a
cadddr = cdr >=> cdr >=> cdr >=> car

kar :: Show a => [a] -> MaybeT IO a
kar a = do
    case (car a) of
        Just x -> do { liftIO $ print x; return x; }
        Nothing -> mzero

kdr :: Show a => [a] -> MaybeT IO [a]
kdr a = do
    case (cdr a) of 
        Just xs -> do { liftIO $ print xs; return xs; }
        Nothing -> mzero

kadddr :: Show a => [a] -> MaybeT IO a
kadddr = kdr >=> kdr >=> kdr >=> kar 


data Person = Bill | Bob | Amy | Val | Jim | Kim deriving (Show, Eq)
ks = [(Bill, [Bob, Amy]), (Bob, [Val, Jim]), (Amy, []), (Val, [Kim])]

kids :: Person -> ListT Maybe Person
kids = ListT . flip lookup ks   

data Pet = Cat | Dog deriving (Show, Eq)
ps = [(Bill, [Dog]), (Bob, [Cat, Dog]), (Val, []), (Kim, [Cat])]

pets :: Person -> ListT Maybe Pet
pets = ListT . flip lookup ps   

grandkidscats :: Person -> ListT Maybe Pet
grandkidscats p = (>=>) kids pets p

item :: Parser Char
item = StateT (\cs -> case cs of
    "" -> Nothing
    (c:cs) -> Just (c, cs))

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


item' :: Parser' Char
item' = ReaderT (\cs -> item)

sat' :: (Char -> Bool) -> Parser' Char
sat' p = do 
     c <- item'
     if p c
     then return c
     else empty

char' :: Char -> Parser' Char
char' c = sat' (c ==)

string' :: String -> Parser' String 
string' "" = return ""
string' (c:cs) = do 
    char' c
    string' cs
    return (c:cs)   

word :: Parser' String
word = do
    dict <- ask
    ident dict    

ident :: [String] -> Parser' String
ident css = do cs <- token identifier
               guard (elem cs css)
               return cs

identifier :: Parser' String
identifier = do {cs <- many alphanum; return (cs)}

alphanum :: Parser' Char
alphanum = sat' isAlphaNum  

success = runStateT (runReaderT word ["hello"]) "hello"
success1 = runStateT (runReaderT word ["hello", "emma"]) "hello"
success2 = runStateT (runReaderT word ["hello", "emma"]) "emma"

fail1 = runStateT (runReaderT word ["hello"]) "emma"


data Beatle = John | Paul | George | Ringo
    deriving (Show, Eq, Enum, Bounded, Read)

data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving Show    

beatles = [(John)..(Ringo)]   

stringBeatles = show <$> beatles

btree :: (Read a, Show a) => [a] -> String -> Btree a
btree d s = fst $ fromJust (apply btreeP (show <$> d) s) 

test = btree beatles "(Fork (Leaf John) (Fork (Fork (Leaf Paul) (Leaf George)) (Leaf Ringo)))"

test1 = "(Fork (Leaf John) (Fork (Fork (Leaf Paul) (Leaf George)) (Leaf Ringo)))"

test2 = "(Fork (Leaf stuff) (Leaf stuff))"

btreeP :: (Read a, Show a) => Parser' (Btree a)
btreeP = do {symb "("; symb "Leaf"; x <- word; symb ")"; return (Leaf (read x))} <|>
         do {symb "("; symb "Fork"; x <- btreeP; y <- btreeP; symb ")"; return (Fork x y)}

apply :: Parser' a -> Dictionary -> String -> Maybe (a, String)
apply p d s = runStateT (runReaderT p d) s

space :: Parser' String
space = many (sat' isSpace)

token  :: Parser' a -> Parser' a
token p = do {a <- p; space; return a}

symb :: String -> Parser' String
symb cs = token (string' cs)




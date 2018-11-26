{-
Kimberly Keller
11/8/2018
Scheme Parser Exercise

Grammer:
symbol ::= first symbolic*
first ::= misc | lower
symbolic ::= first | digit
misc ::= ’<’ | ’>’ | ’ˆ’ | ’+’ | ’-’ | ’*’ | ’/’ | ’=’
number ::= digit+
S ::= "()" | ’(’ E ’)’ | A | ’(’ S ’.’ S ’)’
E ::= ’(’ E ’)’ E | S E | S
A ::= symbol | number
-}

import Parselib
import Control.Applicative hiding (many)

data Sexpr = Symbol String 
           | Number Int 
           | Nil 
           | Cons Sexpr Sexpr

test = "(define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))"

test1 = "()"

instance Show Sexpr where
    show (Symbol x) = x
    show (Number x) = show x
    show Nil = "()"
    show (Cons x y) = "(" ++ show x ++ showCdr y ++ ")"

showCdr :: Sexpr -> String
showCdr Nil = ""
showCdr (Cons x Nil) = " " ++ show x
showCdr (Cons x v@(Cons y z)) = " " ++ show x ++ showCdr v
showCdr (Cons x y) = " " ++ show x ++ " . " ++ show y
showCdr x = " . " ++ show x

symbol :: Parser Sexpr
symbol = do { (Symbol f) <- first; n <- (many alphanum); return (Symbol (f ++ n)) }

first :: Parser Sexpr
first =  misc <|> do { n <- token lower; return (Symbol [n]) }

symbolic :: Parser Sexpr
symbolic = first <|> number

misc :: Parser Sexpr
misc =  do { symb "<"; return (Symbol "<") } <|> 
        do { symb ">"; return (Symbol ">") } <|>
        do { symb "^"; return (Symbol "^") } <|>
        do { symb "+"; return (Symbol "+") } <|>
        do { symb "-"; return (Symbol "-") } <|>
        do { symb "*"; return (Symbol "*") } <|>
        do { symb "/"; return (Symbol "/") } <|>
        do { symb "="; return (Symbol "=") } 

parserS :: Parser Sexpr
parserS = do { symb "()"; return Nil } <|>
          do { symb "("; n <- token parserE; symb ")"; return n} <|>
          parserA <|>
          do { symb "("; x <- token parserS; y <- token parserS; symb ")"; return (Cons x y)}


parserE :: Parser Sexpr
parserE = do { symb "("; n <- token parserE; symb ")"; m <- token parserE; return (Cons n m)} <|>
          do { n <- token parserS; m <- token parserE; return (Cons n m)} <|>
          parserS 

parserA :: Parser Sexpr
parserA = symbol <|> number

number :: Parser Sexpr
number = do { n <- token integer; return (Number n) }


p :: String -> [(Sexpr, String)]
p s = case (apply parserE s) of 
    [] -> []
    (x:xs) -> [x]

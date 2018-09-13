{-
(f >=> g) >=> h  === f >=> (g >=> h)

return >=> f  ===  f >=> return  ===  f
-}


import Control.Monad
import Data.List
import Data.Char

data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving (Eq, Show)

test :: Btree Int
test = Fork (Leaf 1) (Leaf 2)

instance Functor Btree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Fork lt rt) = Fork (fmap f lt) (fmap f rt)

iota' :: [Int] -> Btree Int
iota' [n] = Leaf n
iota' ns = Fork left right
            where 
                left = iota' (fst zs)
                right = iota' (snd zs)
                y = (length ns) `div` 2
                zs = splitAt y ns

iota :: Int -> Btree Int
iota x = iota' [1..x]

                    
instance  Applicative Btree where
    (<*>) = ap
    pure = return 

instance Monad Btree where
    return = Leaf 
    (>>=) (Leaf a) f =  f a
    (>>=) (Fork a b) f = (Fork ((>>=) a f) ((>>=) b f)) 

{- 

To Do: Write up proper test cases for the monad laws 

((iota >=> iota) >=> iota) 4

(iota >=> (iota >=> iota)) 4

(return >=> iota) 4
Fork (Fork (Leaf 1) (Leaf 2)) (Fork (Leaf 3) (Leaf 4))

(iota >=> return) 4
Fork (Fork (Leaf 1) (Leaf 2)) (Fork (Leaf 3) (Leaf 4))

iota 4
Fork (Fork (Leaf 1) (Leaf 2)) (Fork (Leaf 3) (Leaf 4))


functor and monad instance for the btree
monad laws for both

there are different monad instances for Lulz
-}   

newtype Lulz a = Lulz {runLulz :: [[a]]} deriving (Eq, Show)

test1 :: Lulz Int
test1 = Lulz [[1,2], [1,2]]

test2 :: Lulz Int
test2 = Lulz [[1,2,3], 
              [1,2,3], 
              [1,2,3]]


test3 :: Lulz Char
test3 = Lulz [['a', 'b', 'c'], ['a', 'b', 'c'], ['a', 'b', 'c']] 

test4 :: Lulz (Lulz Int)
test4 = Lulz [[Lulz [[1,2,3], 
                     [1,2,3], 
                     [1,2,3]]], 
              [Lulz [[1,2,3], 
                     [1,2,3], 
                     [1,2,3]]],
              [Lulz [[1,2,3], 
                     [1,2,3], 
                     [1,2,3]]]]

test5 :: Lulz [Int]
test5 = (fmap (concat . runLulz) test4)

test6 :: Lulz Int
test6 = (rho >=> (rho >=> rho)) 2

test7 :: Lulz Int
test7 = ((rho >=> rho) >=> rho) 2

test8 :: Lulz Int
test8 = (return >=> rho) 2

test9 :: Lulz Int
test9 = (rho >=> return) 2

instance Functor Lulz where
    fmap f (Lulz a) = Lulz [ [f x | x <- ks] | ks <- a ]

{- [[1,2], [3,4]] -}
rho :: Int -> Lulz Int
rho n = Lulz [ [i | i<-[1..n]] | j <- [1..n] ]


joinLulz :: Lulz (Lulz a) -> Lulz a
joinLulz xs = Lulz (fmap concat (runLulz ks)) where ks = (fmap (concat . runLulz) xs)


joinLulz :: Lulz (Lulz a) -> Lulz a
joinLulz xs = Lulz (concat (fmap runLulz (concat (runLulz xs))))

instance  Applicative Lulz where
    (<*>) = ap
    pure = return 


instance Monad Lulz where
    return a = Lulz [[a]]
    (>>=) lulz f = joinLulz (fmap f lulz)




















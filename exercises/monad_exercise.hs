import Control.Monad
import Data.List

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

newtype Lulz a = Lulz {runLulz :: [[a]]} deriving (Eq, Show)

test2 :: Lulz Int
test2 = Lulz [[1,2,3], 
              [1,2,3], 
              [1,2,3]]

instance Functor Lulz where
    fmap f (Lulz a) = Lulz [ [f x | x <- ks] | ks <- a ]

rho :: Int -> Lulz Int
rho n = Lulz [ [i | i<-[1..n]] | j <- [1..n] ]

instance  Applicative Lulz where
    (<*>) = ap
    pure = return 

instance Monad Lulz where
    return = Lulz
    (>>=) (Lulz a) f = 




















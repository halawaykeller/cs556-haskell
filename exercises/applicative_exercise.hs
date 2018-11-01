{- 
Kimberly Keller
Curtis G
Applicative Exercies
10/2/2018
-}

import Control.Monad
import Control.Monad.Plus
import Control.Applicative 

data List a = Nil | Cons {car :: a, cdr :: List a} deriving (Eq, Show)

data RoseTree a = Node a [RoseTree a] deriving (Eq, Show)

newtype ZipRoseTree a = ZipRoseTree {getZipRoseTree :: RoseTree a} deriving (Eq, Show)

class Zippable z where
    zipWith' :: (a -> b -> c) -> z a -> z b -> z c
    zip :: z a -> z b -> z (a,b)
    zip = zipWith' (,)
    (<+>) :: z (b -> c) -> z b -> z c
    (<+>) = zipWith' ($)
infixl 1 <+>

test1 = Cons (+1) Nil
test2 = [(+1)]

f = Cons (+ 1) (Cons (+ 2) (Cons (+ 3) Nil))
x = Cons 1 (Cons 2 (Cons 3 Nil))
y = Cons 4 (Cons 5 (Cons 6 Nil))

fr = Node (+ 1) [Node (+ 2) [], Node (+ 3) []]
fr' = Node (+1) []

gr = Node (+ 1) [Node (+ 2) []]

xr = Node 1 [Node 2 [], Node 3 []]
xr' = Node 1 [Node 2 [Node 1 [Node 2 [], Node 3 []]], Node 3 [Node 1 [Node 2 [], Node 3 []]]]
xr'' = Node 1 []
xr''' = Node 2 [Node 3 [], Node 4 []]

yr = Node 1 [Node 2 []]


res = Node 2 [Node 3 [],Node 4 [],Node 3 [Node 4 [],Node 5 []],Node 4 [Node 5 [],Node 6 []]]
res' = Node 2 [Node 3 [],Node 3 [Node 4 []]]


-- used for testing
replicate' :: Int -> a -> List a
replicate' 0 _ = Nil
replicate' x a = Cons a (replicate' (x-1) a)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    (<*>) f Nil = Nil
    (<*>) Nil f = Nil
    (<*>) (Cons f fs) xs = mappend (fmap f xs) ((<*>) fs xs)

instance Monoid (List a) where
    mempty = Nil
    mappend a Nil = a
    mappend Nil a = a
    mappend a b = Cons (car a) (mappend (cdr a) b)

instance Foldable List where
    foldr f b Nil = b
    foldr f b a = f (car a) (foldr f b (cdr a))
    foldMap f = foldr (mappend . f) mempty

instance Alternative List where
    (<|>) = mplus
    empty = mzero

instance Monad List where
    return x = Cons x Nil
    (>>=) Nil f = Nil
    (>>=) a f = mappend (f (car a)) ((cdr a) >>= f)

instance MonadPlus List where
    mzero = Nil
    mplus = mappend 

instance Functor RoseTree where
    fmap f (Node a []) = Node (f a) []
    fmap f (Node a roses) = Node (f a) (map (fmap f) roses)

instance Applicative RoseTree where
    pure x = Node x []
    (<*>) (Node f fs) (Node x []) = Node (f x) []
    (<*>) (Node f fs) (Node x xs)= Node (f x) $ ((funcs <*>) <$> xs) `mappend` ((<*> roses) <$> fs)
                                    where funcs = Node f fs
                                          roses = Node x xs

instance Zippable RoseTree where
    zipWith' f (Node a []) (Node b _) = Node (f a b) []
    zipWith' f (Node a _) (Node b []) = Node (f a b) []
    zipWith' f (Node a as) (Node b bs) = Node (f a b) (zipWith (zipWith' f) as bs)

instance Functor ZipRoseTree where
    fmap f (ZipRoseTree a) = ZipRoseTree (fmap f a)

instance Applicative ZipRoseTree where
    pure x = ZipRoseTree (pure x)
    (<*>) (ZipRoseTree fs) (ZipRoseTree as) = ZipRoseTree (fs <+> as)    





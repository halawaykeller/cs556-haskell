import Control.Monad
import Control.Monad.Plus
import Control.Applicative 

data List a = Nil | Cons {car :: a, cdr :: List a} deriving (Eq, Show)

test1 = Cons (+1) Nil
test2 = [(+1)]

f = Cons (+ 1) (Cons (+ 2) (Cons (+ 3) Nil))
x = Cons 1 (Cons 2 (Cons 3 Nil))
y = Cons 4 (Cons 5 (Cons 6 Nil))

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
    foldr = undefined
    foldMap = undefined

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







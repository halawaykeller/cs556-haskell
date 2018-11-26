{-
Kimberly Keller + Jazmine Madrigal

10/31/2018
State Monad Exercise
-}

import Control.Monad
import Data.Char
import qualified System.Random as S

newtype State s a = State {runState :: s -> (a, s)}

data Tree a = Unary (Tree a) | Binary (Tree a) (Tree a)
  | Ternary (Tree a) (Tree a) (Tree a) | Leaf a deriving (Show, Eq, Functor, Foldable, Traversable)

instance Functor (State s) where
    fmap f s = State $ \c ->  
                let (a, s') = runState s c
                in (f a, s')

instance Applicative (State s) where
    pure = return
    (<*>) = ap 

instance Monad (State s) where 
    return a = State $ \s -> (a, s) 
    m >>= f= State $ \s->
        let (a', s') = runState m s
        in runState (f a') s'    

get :: State a a
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

evalState :: State b a -> b -> a
evalState x s = fst $ runState x s

foo = Ternary (Binary (Leaf 1) (Leaf 2)) (Leaf 3) (Ternary (Leaf 5) (Leaf 7) (Binary (Unary (Leaf 11)) (Leaf 13))) 

foo1 = Binary (Unary (Leaf 11)) (Leaf 13)

test = copyM foo :: State a (Tree Integer)

copy :: Tree a -> Tree a
copy (Leaf a) = Leaf a
copy (Unary t) = Unary (copy t)
copy (Binary t1 t2) = Binary (copy t1) (copy t2)
copy (Ternary t1 t2 t3) = Ternary (copy t1) (copy t2) (copy t3)

copyM :: Monad m => Tree a -> m(Tree a)
copyM (Leaf a) = Leaf <$> (pure a)
copyM (Unary t) = Unary <$> (copyM t)
copyM (Binary t1 t2) = Binary <$> (copyM t1) <*> (copyM t2)
copyM (Ternary t1 t2 t3) = Ternary <$> (copyM t1) <*> (copyM t2) <*> (copyM t3)

next :: Enum a => State a a
next = do n <- get
          put (succ n)
          return n

num :: S.RandomGen g => State g Int
num = State $ S.randomR (0, 1) 

label :: Enum b => Tree a -> b -> Tree b
label t b = evalState (sequence $ label' t) b

label' :: Enum b => Tree a -> Tree (State b b)
label' (Leaf a) = Leaf next
label' (Unary t) = Unary (label' t)
label' (Binary t1 t2) = Binary (label' t1) (label' t2)
label' (Ternary t1 t2 t3) = Ternary (label' t1) (label' t2) (label' t3)

randomize :: Tree a -> Tree Int
randomize t = evalState (sequence $ randomize' t) (S.mkStdGen 1)

randomize' :: S.RandomGen g => Tree a -> Tree (State g Int)
randomize' (Leaf a) = Leaf num
randomize' (Unary t) = Unary (randomize' t)
randomize' (Binary t1 t2) = Binary (randomize' t1) (randomize' t2)
randomize' (Ternary t1 t2 t3) = Ternary (randomize' t1) (randomize' t2) (randomize' t3)


{-
Tests:

*Main
λ> label foo 'a'
Ternary (Binary (Leaf 'a') (Leaf 'b')) (Leaf 'c') (Ternary (Leaf 'd') (Leaf 'e') (Binary (Unary (Leaf 'f')) (Leaf 'g')))

*Main
λ> label foo '1'
Ternary (Binary (Leaf '1') (Leaf '2')) (Leaf '3') (Ternary (Leaf '4') (Leaf '5') (Binary (Unary (Leaf '6')) (Leaf '7')))

*Main
λ> randomize foo
Ternary (Binary (Leaf 1) (Leaf 0)) (Leaf 1) (Ternary (Leaf 1) (Leaf 0) (Binary (Unary (Leaf 1)) (Leaf 0))) 
-}
















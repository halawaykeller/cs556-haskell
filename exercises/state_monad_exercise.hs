import Control.Monad
import Data.Char

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

execState :: State b a -> b -> b
execState m s = snd (runState m s)


foo = Ternary (Binary (Leaf 1) (Leaf 2)) (Leaf 3) (Ternary (Leaf 5) (Leaf 7) (Binary (Unary (Leaf 11)) (Leaf 13))) 

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

label' :: Enum a1 => Tree a -> a1 -> Tree a1
label' t b = evalState (label t b) b

test1 :: Tree Char
test1 = label' foo 'a'

next :: Enum a => State a a
next = do n <- get
          put (succ n)
          return n

succ' :: Enum a => a -> a
succ' n = execState next n

-- plus :: Int -> Int -> Int
-- plus n x = execState (sequence $ replicate n tick) x           

label :: (Applicative f, Enum a1) => Tree a2 -> a1 -> f (Tree a1)
label (Leaf a) b = Leaf <$> pure (succ' b)
label t b = label t (succ b)


-- label (Unary t) b = Unary <$> (label t (succ' b))
-- label (Binary t1 t2) b = Binary <$> (label t1 (succ' b)) <*> (label t2 (succ' b))
-- label (Ternary t1 t2 t3) b = Ternary <$> (label t1 (succ' b)) <*> (label t2 (add (succ' b))) <*> (label t3 (succ' b))


-- create a solution using traverese for problem 4, create a tree of state monads and create a state monad of tree and then runstate to apply to a seed
-- tree of random to random tree
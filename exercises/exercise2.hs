data List a = Nil | Cons {car :: a, cdr :: List a}

instance Functor List where
    fmap = undefined

instance Applicative List where
    func =  

instance Monoid List where
    func = 

instance Foldable List where
    func = 

instance Monad List where
    func = 

instance MonadPlus List where
    func =      
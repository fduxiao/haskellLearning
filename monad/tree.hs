data Tree a = Tip a | Bin (Tree a) (Tree a) deriving (Show)
instance Monad Tree where
   return = Tip
   Tip a >>= f = f a
   Bin l r >>= f = Bin (l >>= f) (r >>= f)


ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = do{f <- mf; a <- ma; return $ f a}

instance Applicative Tree where
    pure = return
    (<*>) = ap

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ma >>= \a -> return $ f a

instance Functor Tree where
    fmap = liftM

sixTree :: Tree Int
sixTree = do
    b1 <- return 0
    b2 <- Bin (Bin (Tip 1) (Tip 2)) (Tip 7)
    Bin (return b1) (return b2)

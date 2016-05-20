data Sheep = Sheep Sheep Sheep|Cloned|Ancestor deriving (Show, Eq)

father :: Sheep -> Maybe Sheep
father (Sheep f m) = Just f
father Cloned = Nothing
father Ancestor = Just Ancestor

mother :: Sheep -> Maybe Sheep
mother (Sheep f m) = Just m
mother Cloned = Nothing
mother Ancestor = Just Ancestor

maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = case mother s of
                          Nothing -> Nothing
                          Just m  -> father m

gf = Sheep Ancestor Ancestor
gm = Ancestor
f = Sheep gf gm
m = Cloned
s = Sheep f m

-- comb is a combinator for sequencing operations that return Maybe
comb :: Maybe a -> (a -> Maybe b) -> Maybe b
comb Nothing  _ = Nothing
comb (Just x) f = f x

-- now we can use `comb` to build complicated sequences
mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = Just s `comb` mother `comb` father `comb` father

-- use Maybe as a monad
{-
instance Monad Maybe where
  Nothing >>= f = Nothing
  (Just x) >>= f = f x
  return = Just
-}
mothersPaternalGrandfather' :: Sheep -> Maybe Sheep
mothersPaternalGrandfather' s = mother s >>= father >>= father

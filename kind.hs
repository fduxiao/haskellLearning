-- * -> *
class Functor2 f where
    fmap :: (a -> b) -> f a -> f b

-- * -> (* -> *) -> *
class Tofu t where
  -- assume * for a, inferring j :: * -> *
  -- so that t :: * -> (* -> *) -> *
  -- thus Tofu is * -> (* -> *) -> * -> constraints
  tofu :: j a -> t a j

--   :k           :t
data Frank a b  = Frank {frankField :: b a} deriving (Show)

{- examples
ghci> :t Frank {frankField = Just "HAHA"}
Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
ghci> :t Frank {frankField = Node 'a' EmptyTree EmptyTree}
Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree
ghci> :t Frank {frankField = "YES"}
Frank {frankField = "YES"} :: Frank Char []
-}

-- we make an instance for typeclass Tofu
instance Tofu Frank where
  -- say Just 'a' for x then we get a value of type Frank Char Maybe
  -- tofu x = Frank x
  tofu = Frank

{- examples
ghci> tofu (Just 'a') :: Frank Char Maybe
Frank {frankField = Just 'a'}
ghci> tofu ["HELLO"] :: Frank [Char] []
Frank {frankField = ["HELLO"]}
-}

data Barry t k p = Barry { yabba :: p, dabba :: t k }
instance Functor2 (Barry a b) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f Barry {yabba = x, dabba = y} = Barry {yabba = f x, dabba = y}

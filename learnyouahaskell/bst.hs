data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right

data KeyValue k v = KeyValue k v deriving (Show)
type BSTree k v = Tree (KeyValue k v)
data BSTResult v = Empty | Value v deriving(Show, Eq)

instance (Eq k) => Eq (KeyValue k v) where
  (KeyValue k1 _) == (KeyValue k2 _) = k1 == k2

instance (Ord k) => Ord (KeyValue k v) where
  (KeyValue k1 _) `compare` (KeyValue k2 _) = k1 `compare` k2

bstUpdate :: (Ord k) => KeyValue k v -> BSTree k v -> BSTree k v
bstUpdate = treeInsert

bstSearch :: (Ord k) => k -> BSTree k v -> BSTResult v
bstSearch k EmptyTree = Empty
bstSearch k (Node (KeyValue a v) left right)
  | k == a = Value v
  | k < a  = bstSearch k left
  | k > a  = bstSearch k right

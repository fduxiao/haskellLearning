allpair :: Int -> [[Bool]]
allpair 1 = [[True], [False]]
allpair n = map (True:) (allpair (n-1)) ++ map (False:) (allpair (n-1))

table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table f = [(x, y, f x y)| [x, y] <- allpair 2]

-- applied both to question 46 and question 47

tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = [xs ++ [f xs]| xs <- allpair n]

gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n = let xs = gray (n-1) in map ('0':) xs ++ map ('1':) (reverse xs)

data Tree a b = Node b (Tree a b) (Tree a b) | Leaf a b deriving(Show)
treeWeight :: (Num b) => Tree a b -> b
treeWeight (Leaf _ w) = w
treeWeight (Node w _ _) = w

mergeTree :: (Num b, Ord b) => Tree a b -> Tree a b -> Tree a b
mergeTree t1 t2 = if w1 < w2 then Node s t1 t2 else Node s t2 t1
  where w1 = treeWeight t1; w2 = treeWeight t2; s = w1 + w2

quickSortCode :: (Num b, Ord b) => [(a, b)] -> [(a, b)]
quickSortCode [] = []
quickSortCode ((x,w):xs) = quickSortCode smaller ++ [(x,w)] ++ bigger
  where smaller = [(i, w2)|(i, w2) <- xs, w2 <= w]; bigger = [(i, w2)|(i, w2) <- xs, w2 > w]

quickSortTree :: (Num b, Ord b) => [Tree a b] -> [Tree a b]
quickSortTree [] = []
quickSortTree (x:xs) = quickSortTree smaller ++ [x] ++ bigger
  where
    w = treeWeight x
    smaller = [i |i <- xs, treeWeight i <= w]
    bigger = [i |i <- xs, treeWeight i > w]

buildLeaves :: (Num b, Ord b) => [(a, b)] -> [Tree a b]
buildLeaves = map (uncurry Leaf)

treeFromLeaves :: (Num b, Ord b) => [Tree a b] -> [Tree a b]
treeFromLeaves [x] = [x]
treeFromLeaves (x1:x2:xs) = treeFromLeaves . quickSortTree $ mergeTree x1 x2:xs

buildTree :: (Num b, Ord b) => [(a, b)] -> Tree a b
buildTree = head . treeFromLeaves . quickSortTree . buildLeaves

decodeTree :: (Num b, Ord b) => String -> Tree a b -> [(a, String)]
decodeTree p (Leaf x _) = [(x, p)]
decodeTree p (Node _ l r) = decodeTree (p ++ "0") l ++ decodeTree (p ++ "1") r

huffman :: (Num b, Ord b) => [(a, b)] -> [(a, String)]
huffman = decodeTree "" . buildTree

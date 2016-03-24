-- tree model provided by the questions
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
-- a short hand function
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

twoStep1 :: (Integral a) => a -> (a, a)
twoStep1 x = ((x - 1) `quot` 2, (x + 1) `quot` 2)

{- which is implemented by myself, with errors
cbalTree :: (Integral a) => a -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = if even (n-1) then
    map (\x -> parent x x) half
  else wholeTree l r ++ wholeTree r l
  where parent = Branch 'x'; half = cbalTree $ (n-1) `quot` 2
        (s, b) = twoStep1 (n - 1); l = cbalTree s; r = cbalTree b
        halfTree = map parent
        flistOnList xs = foldr (\f acc -> acc ++ map f xs) []
        wholeTree l r = flistOnList r $ halfTree l
-}
-- the official solution is simpler
cbalTree :: (Integral a) => a -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
    in [Branch 'x' left right | i     <- [q .. q + r],
                                left  <- cbalTree i,
                                right <- cbalTree (n - i - 1)]

mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Branch x l r) = Branch x (mirror r) (mirror l)

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = sameTree l (mirror r)
  where sameTree Empty Empty = True
        sameTree Empty Branch{} = False
        sameTree Branch{} Empty = False
        sameTree (Branch _ l1 r1) (Branch _ l2 r2) = sameTree l1 l2 && sameTree r1 r2
{- official solution is still simpler :(
mirror Empty          Empty          = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _              _              = False

symmetric Empty          = True
symmetric (Branch _ l r) = mirror l r
--Even simpler is

symmetric t = mirror t t
-}

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Empty = leaf x
treeInsert x (Branch a left right)
  | x == a = Branch x left right
  | x < a  = Branch a (treeInsert x left) right
  | x > a  = Branch a left (treeInsert x right)

construct :: (Ord a) => [a] -> Tree a
construct = foldl (flip treeInsert) Empty

symCbalTrees :: (Integral a) => a -> [Tree Char]
symCbalTrees n = [one |one <- cbalTree n, symmetric one]
{- -- a simpler and efficient solution
symCbalTrees n = if n `mod` 2 == 0 then [] else
    [ Branch 'x' t (reverseTree t) | t <- cbalTree (n `div` 2)]

reverseTree Empty = Empty
reverseTree (Branch x l r) = Branch x (reverseTree r) (reverseTree l)
-}

-- I hadn't understood what is a height balanced tree,
-- so I read the anwser. Here it is.
hbaltree :: a -> Int -> [Tree a]
hbaltree x 0 = [Empty]
hbaltree x 1 = [Branch x Empty Empty]
hbaltree x h = [Branch x l r |
        (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],
        l <- hbaltree x hl, r <- hbaltree x hr]

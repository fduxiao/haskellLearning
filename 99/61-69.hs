-- tree module used in the following
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch _ l r) = leaves l ++ leaves r

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch a l r) = a:(internals l ++ internals r)

atLevel :: Tree a -> Int -> [a]
atLevel (Branch a _ _) 1 = [a]
atLevel (Branch _ l r) n = atLevel l (n-1) ++ atLevel r (n-1)
atLevel _ _ = []

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = generate_tree 1
  where generate_tree x
          | x > n     = Empty
          | otherwise = Branch 'x' (generate_tree (2*x)  )
                                   (generate_tree (2*x+1))

tree64 = Branch 'n'
               (Branch 'k'
                       (Branch 'c'
                               (Branch 'a' Empty Empty)
                               (Branch 'h'
                                       (Branch 'g'
                                               (Branch 'e' Empty Empty)
                                               Empty
                                       )
                                       Empty
                               )
                       )
                       (Branch 'm' Empty Empty)
               )
               (Branch 'u'
                       (Branch 'p'
                               Empty
                               (Branch 's'
                                       (Branch 'q' Empty Empty)
                                       Empty
                               )
                       )
                       Empty
               )

width :: Tree a -> Int
width Empty = 0
width (Branch _ l r) = 1 + width l + width r

layout :: Tree a -> Tree (a, (Int, Int))
layout = layoutOffset (0, 1) where
  layoutOffset _ Empty = Empty
  layoutOffset (x, y) (Branch a l r) = let lw = width l in
    Branch (a, (x + lw + 1, y)) (layoutOffset (x, y+1) l) (layoutOffset(x + lw + 1, y+1) r)

-- official solution, which is more efficient
type Pos = (Int, Int)
layout' :: Tree a -> Tree (a, Pos)
layout' t = fst (layoutAux 1 1 t)
  -- (Arranged Tree, the position of the rightest element of it)
  where layoutAux x y Empty = (Empty, x)
        layoutAux x y (Branch a l r) = (Branch (a, (x',y)) l' r', x'')
          where (l', x')  = layoutAux x (y+1) l
                (r', x'') = layoutAux (x'+1) (y+1) r

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

-- alright I give up, which is too complex
layout65 :: Tree a -> Tree (a, (Int, Int))
layout65 t = let (r, _, _) = layout65Aux 1 1 t in r where
  layout65Aux x _ Empty = (Empty, x, 1)
  layout65Aux x y (Branch a l r) = (Branch (a, (lp, y)) l' r', lp + w, w * 2) where
    (l', lp, w) = layout65Aux x (y+1) l
    (r', rp, _) = layout65Aux (x+w) (y+1) r

-- alright, I skip 66 it's a little difficult and I think I solve it but I don't want to implement it

tree2String :: Tree Char -> String
tree2String Empty = ""
tree2String (Branch x Empty Empty) = [x]
tree2String (Branch x l r) = x:'(':tree2String l ++ "," ++ tree2String r ++ ")"


string2Tree :: String -> Tree Char
string2Tree s = fst . parser $ s where
  parser [] = (Empty, "")
  parser [')'] = (Empty, "")
  parser (x:y:r)
    | y == '(' = let
      (left, r1) = parser (y:r); (right, r2) = parser r1
      in (Branch x left right, r2)
    | y == ',' = case x of
      '(' -> (Empty, r)
      ')' -> parser r
      otherwise -> (Branch x Empty Empty, r)
    | y == ')' = case x of
      ',' -> (Empty, r)
      ')' -> (Empty, y:r)
      otherwise -> (Branch x Empty Empty, r)
    | otherwise = parser (y:r)

{- the official solution uses Monad, which I have no idea currently
stringToTree :: (Monad m) => String -> m (Tree Char)
stringToTree "" = return Empty
stringToTree [x] = return $ Branch x Empty Empty
stringToTree str = tfs str >>= \ ("", t) -> return t
    where tfs a@(x:xs) | x == ',' || x == ')' = return (a, Empty)
          tfs (x:y:xs)
                | y == ',' || y == ')' = return (y:xs, Branch x Empty Empty)
                | y == '(' = do (',':xs', l) <- tfs xs
                                (')':xs'', r) <- tfs xs'
                                return $ (xs'', Branch x l r)
          tfs _ = fail "bad parse"
-}

-- the following require Monad, which I have no idea :(

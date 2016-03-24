insertAt :: a -> [a] -> Int -> [a]
insertAt a xs 1 = a:xs
insertAt x [] i | i < 1 = [x]
insertAt a (x:xs) i = x:insertAt a xs (i-1)

range :: (Num a, Ord a, Eq a) => a -> a -> [a]
range a b
  | a == b = [a]
  | a < b = a:range (a+1) b
  | a > b = a:range (a-1) b

-- 23 24 25 require random which I don't want to do

-- scaffold for combinations
through ::  (b -> [a] -> b) -> b -> [a] -> b
through _ i []  = i
through f i a@(x:xs) = f (through f i xs) a

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 (x:xs) = [x]:combinations 1 xs
combinations i cc = through (\acc (x:xs) -> [x:one | one <- combinations (i-1) xs] ++ acc) [] cc

-- this is a solution provided by the official website
-- which i think is more mathematical
combinations2 :: Int -> [a] -> [[a]]
combinations2 _ [] = []
combinations2 1 xs = [[x] | x<-xs]
combinations2 n (x:xs) = map (x:) (combinations2 (n-1) xs) ++ combinations2 n xs

-- all right, I read the answer
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs =
    [ g:gs | (g,rs) <- combination n xs
           ,  gs    <- group ns rs ]

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (xs:xss) = smaller ++ [xs] ++ bigger
  where
    smaller = lsort [x | x <- xss, length x <= length xs]
    bigger = lsort [x | x <- xss, length x > length xs]

freq :: [a] -> [[a]] -> Int
freq _ [] = 0
freq o (x:xs) = freq o xs + if length o == length x then 1 else 0

lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort a = lfsort' a a
  where
    lfsort' :: [[a]] -> [[a]] -> [[a]]
    lfsort' [] _ = []
    lfsort' (xs:xss) al = smaller ++ [xs] ++ bigger
      where
        smaller = lfsort' [x | x <- xss, freq x al <= freq xs al] al
        bigger = lfsort' [x | x <- xss, freq x al > freq xs al] al

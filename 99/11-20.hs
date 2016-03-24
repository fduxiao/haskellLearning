data Result11 a = Multiple Int a | Single a | Zero a deriving(Show)

addOne :: Result11 a -> Result11 a
addOne (Zero a) = Single a
addOne (Single a) = Multiple 2 a
addOne (Multiple n a) = Multiple (n + 1) a

hasV :: (Eq a) => a -> Result11 a -> Bool
hasV v (Zero a) = v == a
hasV v (Single a) = v == a
hasV v (Multiple _ a) = v == a


encodeModified :: Eq a => [a] -> [Result11 a]
encodeModified xs = foldr (\x acc@(s:r)-> if hasV x s then addOne s:r else Single x:acc) [Zero (last xs)] xs

decodeModified :: [Result11 a] -> [a]
decodeModified = concatMap decodeHelper
    where
      decodeHelper (Single a) = [a]
      decodeHelper (Multiple n a) = replicate n a


-- problem 13 already satisfied


dupli :: [x] -> [x]
dupli [] = []
dupli [x] = [x, x]
dupli (x:xs) = x:x:dupli xs

repli :: [x] -> Int -> [x]
repli [] _ = []
repli [x] n = replicate n x
repli (x:xs) n = replicate n x ++ dupli xs

dropEvery :: [x] -> Int -> [x]
dropEvery xs n = helper xs n
    where helper [] _ = []
          helper (x:xs) 1 = helper xs n
          helper (x:xs) k = x : helper xs (k-1)

split :: [x] -> Int -> ([x], [x])
split = flip splitAt

slice :: [x] -> Int -> Int -> [x]
slice xs s e = take (e - s + 1) . drop (s - 1) $ xs

rotate :: [x] -> Int -> [x]
rotate xs n = let l = length xs; rn = n `mod` l in drop rn xs ++ take rn xs

removeAt :: Int -> [x] -> (x, [x])
removeAt n xs = (xs !! (n - 1), take (n-1) xs ++ drop n xs)

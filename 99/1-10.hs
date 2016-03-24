solv1 :: [x] -> x
solv1 [] = error "Empty List"
solv1 [x] = x
solv1 (_:xs) = solv1 xs

solv2 :: [x] -> x
solv2 [] = error "Empty List"
solv2 [_] = error "Inadquent arguments"
solv2 (x:[_]) = x
solv2 (_:xs) = solv2 xs

solv3 :: [x] -> Int -> x
solv3 [] _ = error "out of range"
solv3 (x:xs) n
    | n <= 0 = error "You can't do it"
    | n == 1 = x
    | otherwise = solv3 xs (n-1)

solv4 :: Num a => [x] -> a
solv4 [] = 0
solv4 (_:xs) = 1 + solv4 xs

solv5 :: [x] -> [x]
solv5 = foldl (flip(:)) []

solv6 :: Eq x => [x] -> Bool
solv6 [_] = True
solv6 (x:xs)
    | x == last xs = solv6 $ init xs
    | otherwise = False

data NestedList a = Elem a | List [NestedList a]
solv7 :: NestedList a -> [a]
solv7 (Elem a) = [a]
solv7 (List xs) = concatMap solv7 xs

solv8 :: Eq a => [a] -> [a]
solv8 xs = foldr (\x acc-> if x == head acc then acc else x:acc ) [last xs] xs

solv9 :: Eq a => [a] -> [[a]]
solv9 [] = [[]]
solv9 [x] = [[x]]
solv9 (x:xs@(y:_)) = let rt@(f:s) = solv9 xs in if x == y then (x:f):s else [x]:rt

solv10 :: Eq a => [a] -> [(Int, a)]
solv10 xs = foldr (\x acc@((n, a):r)-> if x == a then (n + 1, a):r else (1, x):acc) [(0, last xs)] xs

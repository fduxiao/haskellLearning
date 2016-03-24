isPrime :: (Integral a) => a -> Bool
isPrime 1 = False
isPrime x = isPrime' x 2
  where
    isPrime' :: (Integral a) => a -> a -> Bool
    isPrime' s x
      | s == x = True
      | otherwise = (s `mod` x) /= 0 && isPrime' s (x+1)

myGCD :: (Integral a) => a -> a -> a
myGCD x y
  | rest == 0 = y
  | otherwise = myGCD y rest
  where rest = x `mod` y

coprime :: (Integral a) => a -> a -> Bool
coprime x y = gcd x y == 1

totient :: (Integral a) => a -> Int
totient x = length [i | i <- [1..x], coprime i x]

primeFactors :: (Integral a) => a -> [a]
primeFactors x = primeFactors' x 2
  where
    primeFactors' :: (Integral a) => a -> a -> [a]
    primeFactors' 1 _ = []
    primeFactors' x i
      | x `mod` i == 0 = i:primeFactors' (x `quot` i) i
      | otherwise = primeFactors' x (i+1)

prime_factors_mult :: (Integral a) => a -> [(a, Int)]
prime_factors_mult 1 = []
prime_factors_mult x = prime_factors_mult' x 2
  where
    prime_factors_mult' :: (Integral a) => a -> a -> [(a, Int)]
    prime_factors_mult' 1 i = [(i, 0)]
    prime_factors_mult' x i
      | x `mod` i == 0 =
        let a@((y, n):xs) = prime_factors_mult' (x `quot` i) i in
          if y == i then (y, n+1):xs else (i, 1):a
      | otherwise = prime_factors_mult' x (i+1)


phi :: (Integral a) => a -> a
-- phi x = foldr (\(p, n) acc -> acc * (p-1) * p ** (n-1)) 1 (prime_factors_mult x)
phi = ss . prime_factors_mult
  where
    ss :: (Integral a) => [(a, Int)] -> a
    ss [] = 1
    ss ((p, n):xs) = (p-1) * p ^ (n-1) * ss xs

primesR :: (Integral a) => a -> a -> [a]
primesR x y = [i | i <- [x..y], isPrime i]
{-
primesR :: Integral a => a -> a -> [a]
primesR a b | even a = filter isPrime [a+1,a+3..b]
            | True   = filter isPrime [a,a+2..b]
-}

sieve :: (Integral a) => [a] -> [a]
sieve [] = []
sieve (n:ns) = n:sieve [ m | m <- ns, m `mod` n /= 0 ]

cn2 :: [a] -> [(a,a)]
cn2 [x] = [(x, x)]
cn2 a@(x:xs) = [(x, y)| y <- a] ++ cn2 xs

goldbach :: (Integral a) => a -> (a, a)
goldbach x = let pairs = cn2 . sieve $ [2..x-1] in head [(x1, x2) | (x1, x2) <- pairs, x1 + x2 == x]
{-  where
    getone :: (Integral a) => [(a, a)] -> (a, a)
    getone ((x1, x2):xs) = if x1 + x2 == x then (x1, x2) else getone xs-}

goldbachList :: (Integral a) => a -> a -> [(a,a)]
goldbachList s b = [goldbach i | i <- [s..b], i `mod` 2 == 0]

goldbachList' :: (Integral a) => a -> a -> a -> [(a,a)]
goldbachList' s b t = [(x, y)|i <- [s..b], i `mod` 2 == 0, let (x, y) = goldbach i, x > t && y > t]

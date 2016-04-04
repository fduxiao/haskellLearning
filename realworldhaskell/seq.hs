-- seq :: a -> t -> t
-- seq forces its first arg to be evaluated and return the second arg directly
foldl' _    zero []     = zero
foldl' step zero (x:xs) =
    let new = step zero x
    in  new `seq` foldl' step new xs

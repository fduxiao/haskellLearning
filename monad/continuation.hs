import Control.Monad
import Control.Monad.Trans.Cont

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

-- We assume CPS versions of the add and square primitives,
-- (note: the actual definitions of add_cps and square_cps are not
-- in CPS form, they just have the correct type)

addCPS :: Int -> Int -> ((Int -> r) -> r)
-- addCPS x y = \k -> k (add x y)
addCPS x y k = k (add x y)

squareCPS :: Int -> ((Int -> r) -> r)
-- squareCPS x = \k -> k (square x)
squareCPS x k = k (square x)

pythagorasCPS :: Int -> Int -> ((Int -> r) -> r)
-- pythagorasCPS x y = \k ->
--  squareCPS x $ \x_squared ->
--  squareCPS y $ \y_squared ->
--  addCPS x_squared y_squared $ k
pythagorasCPS x y k =
 squareCPS x $ \x_squared ->
 squareCPS y $ \y_squared ->
 addCPS x_squared y_squared k

-- without CPS
thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

-- A higher order function such as thrice, when converted to CPS,
-- takes as arguments functions in CPS form as well.
-- Therefore, f :: a -> a will become f_cps :: a -> ((a -> r) -> r),
-- and the final type will be thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r).
-- The rest of the definition follows quite naturally from the types -
-- we replace f by the CPS version, passing along the continuation at hand.
thriceCPS :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thriceCPS f_cps x k =
 f_cps x $ \fx ->
 f_cps fx $ \ffx ->
 f_cps ffx k

-- Then we need to give a neat way to compose Continuations
chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
-- s is the result of one calculation
-- f is a new calculation
-- so they yield another calucation result
chainCPS s f k = s $ \a -> f a k

-- Then we make this into a Monad
-- cont :: ((a -> r) -> r) -> Cont r a
-- runCont :: Cont r a -> (a -> r) -> r
-- instance Monad (Cont r) where
--    return x = cont ($ x)
--    s >>= f  = cont $ \c -> runCont s $ \x -> runCont (f x) c
-- so let's accomplish the pytagoras CPS into Cont
addCont :: Int -> Int -> Cont r Int
addCont x y = return $ x + y

squareCont :: Int -> Cont r Int
squareCont x = return $ x * x

pythagorasCont :: Int -> Int -> Cont r Int
pythagorasCont x y = do
    x_squared <- squareCont x
    y_squared <- squareCont y
    addCont x_squared y_squared

-- Without callCC
-- square :: Int -> Cont r Int
-- square n = return (n ^ 2)

-- With callCC
-- The argument passed to callCC is a function, whose result is a suspended computation 。
-- k is a function which acts as an eject button:
-- calling it anywhere will lead to the value passed to it
-- being made into a suspended computation,
-- which then is inserted into control flow at the point of the callCC invocation
-- callCC :: (Monad m) => ((a -> m b) -> m a) -> m a
-- callCC f = Cont $ \h -> runCont (f (\a -> Cont $ \_ -> h a)) h
-- the function (\a -> Cont $ \_ -> h a) will be the exiting function k
-- shadowing everying behind it and yielding the result by feeding the argument
-- applied to it to next Monad
squareCCC :: Int -> Cont r Int
squareCCC n = callCC $ \k -> k (n ^ 2)

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
    let y = x ^ 2 + 3
    when (y > 20) $ k "over twenty"
    return (show $ y - 4)

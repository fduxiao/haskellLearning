data Pause m
  = Run (m (Pause m))
  | Done

pauseExample1 :: Pause IO
pauseExample1 = Run $ do
  putStrLn "Let's begin"
  putStrLn "Step 1"
  return $ Run $ do
    putStrLn "Step 2"
    return $ Run $ do
      putStrLn "Step 3"
      putStrLn "Yay, we're done!"
      return Done

runN :: Monad m => Int -> Pause m -> m (Pause m)
runN 0 p = return p
runN _ Done = return Done
runN n (Run m)
  | n < 0     = fail "Invalid argument to runN" -- ewww I just used fail.
  | otherwise = m >>= runN (n - 1)

fullRun :: Monad m => Pause m -> m ()
fullRun Done = return ()
fullRun (Run m) = m >>= fullRun

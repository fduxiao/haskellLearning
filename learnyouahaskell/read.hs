import System.IO

main :: IO ()
main = do
  handle <- openFile "bst.hs" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

{-
  main = do
      withFile "girlfriend.txt" ReadMode (\handle -> do
          contents <- hGetContents handle
          putStr contents)
-}

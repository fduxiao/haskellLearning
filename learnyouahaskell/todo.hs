import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            , ("bump", bump)
            ]

main :: IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = return ()

int2str :: Int -> String
int2str = show

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> int2str n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks
view _ = return ()

readAndModifyInt :: [String] -> (Int -> [String] -> [String]) -> IO ()
readAndModifyInt [fileName, numberString] f = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString :: Int
      todoTasks = lines contents
      newTodoItems = f number todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
readAndModifyInt _ _ = return ()

remove :: [String] -> IO ()
remove xs =
  readAndModifyInt xs (\number todoTasks -> delete (todoTasks !! number) todoTasks)

move2top :: Int -> [String] -> [String]
move2top 0 xs = xs
move2top n (x:xs) = move2top (n-1) (xs ++ [x])
move2top _ _ = []

bump :: [String] -> IO ()
bump xs =
  readAndModifyInt xs move2top

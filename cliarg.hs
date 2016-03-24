import System.Environment
import Data.List()

main :: IO ()
main = do
   args <- getArgs
   progName <- getProgName
   putStrLn "The arguments are:"
   mapM_ putStrLn args
   putStrLn "The program name is:"
   putStrLn progName

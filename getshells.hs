import Data.List (group, reverse, sort)
import System.IO ()

main :: IO ()
main = do
  contents <- readFile "passwd"
  let counts = map count $ group $ sort $ map getShell $ lines contents
  let output = map (\(shell, n) -> shell ++ ": " ++ show n) counts

  mapM_ putStrLn output

getShell :: String -> String
getShell = takeWhile (/= '\r') . reverse . takeWhile (/= ':') . reverse

count :: [String] -> (String, Int)
count l = (head l, length l)
import Control.Exception
import Data.List (group, reverse, sort)
import Data.Map
import System.CPUTime
import System.IO ()
import Text.Printf

main :: IO ()
main = do
  start <- getCPUTime
  contents <- readFile "passwd"
  let counts = map count $ group $ sort $ map getShell $ lines contents
  let output = map (\(shell, n) -> shell ++ ": " ++ show n) counts
  let shells = map getShell $ lines contents

  mapM_ putStrLn output
  end <- getCPUTime

  let diff = (fromIntegral (end - start)) / (10 ^ 12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)

getShell :: String -> String
getShell = takeWhile (/= '\r') . reverse . takeWhile (/= ':') . reverse

count :: [String] -> (String, Int)
count l = (head l, length l)

count' :: Map -> [String] -> Map
count' m xs = m
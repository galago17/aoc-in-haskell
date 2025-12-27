apply :: String -> [Int] -> Int
apply operation nums
  | operation == "*" = product nums
  | operation == "+" = sum nums

getCol :: Int -> [[Int]] -> [Int]
getCol coln = map (head . drop (coln - 1))

getCols :: [[Int]] -> [[Int]]
getCols lst = map (`getCol` lst) [1..len]
  where len = length $ head lst

applyAll :: [[Int]] -> [String] -> [Int]
applyAll [] _ = []
applyAll xs operations =
  apply (head operations) (head xs) : applyAll (tail xs) (tail operations) 

main :: IO ()
main = do
  contents <- readFile "test.txt"
  let file = map words $ lines contents
  let operations = last file
  let numbers = map (map read) $ init file :: [[Int]]
  -- print $ getCols numbers
  -- print operations
  print $ sum $ applyAll (getCols numbers) operations




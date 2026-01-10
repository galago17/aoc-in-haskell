apply :: String -> [Int] -> Int
apply operation nums
  | operation == "*" = product nums
  | operation == "+" = sum nums
getCol :: [String] -> String
getCol = map last

getCols :: [String] -> [String]
getCols file
  | any not (map null file) = getCol file : getCols (map init file)
  | otherwise = []
hasNums :: String -> Bool
hasNums = any (\x -> x `elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'])

getProblem :: [String] -> [Int]
getProblem cols = map read $ map (filter (/= '.')) (takeWhile hasNums cols) :: [Int]

getProblems :: [String] -> [[Int]]
getProblems cols
  | null cols = []
  | otherwise = getProblem cols : (getProblems $ drop 1 $ dropWhile hasNums cols)

applyAll :: [[Int]] -> [String] -> [Int]
applyAll [] _ = []
applyAll xs operations =
  apply (head operations) (head xs) : applyAll (tail xs) (tail operations)

main :: IO ()
main = do
  contents <- readFile "input.txt"

  -- convert to list of lines, replace all spaces with periods for visibility
  let file = map (map (\x -> if x == ' ' then '.' else x)) (init (lines contents))
  let operations = reverse $ words $ last (lines contents)
  -- print file
  -- print operations 
  let cols = getCols file
  let problems = getProblems cols
  -- print problems
  print $ sum $ applyAll problems operations

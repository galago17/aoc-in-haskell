import Data.List (sortOn)

splitOnDash :: String -> (Int, Int)
splitOnDash str = (lower, upper) 
  where 
    split = break (== '-') str 
    lower = read $ fst split :: Int
    upper = read $ tail $ snd split :: Int
    
inRange :: Int -> (Int, Int) -> Bool
inRange val (lower, upper) = 
    val <= upper
    
separateRanges :: [(Int, Int)] -> [(Int, Int)]
separateRanges ranges
  | length ranges == 1 = ranges
  | fst next `inRange` current && snd next `inRange` current = separateRanges (current : drop 2 ranges)
  | fst next `inRange` current = current: separateRanges ((snd current + 1, snd next) : drop 2 ranges)
  | otherwise = current : separateRanges (tail ranges)
  where
    next = ranges !! 1
    current = head ranges

countRange :: (Int, Int) -> Int
countRange (x, y) = (y - x) + 1

main :: IO () 
main = do
  contents <- readFile "input.txt"
  let rangeStrs = takeWhile (/= "") (lines contents)
  let total = sum $ map countRange $ separateRanges $ sortOn fst $ map splitOnDash rangeStrs
  print total

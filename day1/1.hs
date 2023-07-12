import System.IO
import Data.List

listStringToListInt :: [String] -> [Int]
listStringToListInt [] = []
listStringToListInt (x:xs) = (read x) : listStringToListInt xs

sumOfCalories :: Num a => [a] -> a 
sumOfCalories [] = 0
sumOfCalories (x:xs) = x + sumOfCalories xs

parseLines :: [String] -> [[String]]
parseLines [] = []
parseLines xs = case span (/= "") xs of 
  (first, []) -> [first]
  (first, ("": last)) -> first : parseLines last

parseFile :: String -> [[String]]
parseFile file = parseLines fileLines where fileLines = lines file

solve :: String -> Int
solve file = maximum $ map (sumOfCalories . listStringToListInt) fileLines 
  where fileLines = parseFile file


main = do
  fileString <- readFile "input.txt"
  let solution = solve fileString
  print solution 

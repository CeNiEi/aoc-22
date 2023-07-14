import qualified Data.Set as Set
import Data.Char

divide :: String -> (String, String)
divide line = splitAt ind line where ind = length line `div` 2

common :: (String, String) -> Char
common (xs, ys) = head . Set.toList $ Set.intersection (Set.fromList xs) (Set.fromList ys)

toPriority :: Char -> Int
toPriority x | x `elem` ['a'..'z'] = ord x - ord 'a' + 1
toPriority x | x `elem` ['A'..'Z'] = ord x - ord 'A' + 27

parseLine :: String -> Int
parseLine = toPriority . common . divide

solve :: String -> Int
solve file = foldl (+) 0 (map parseLine $ lines file)

main = do 
  fileString <- readFile "input.txt"
  let solution = solve fileString
  print solution

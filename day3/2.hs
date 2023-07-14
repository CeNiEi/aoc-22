import qualified Data.Set as Set
import Data.Char

divide :: String -> (String, String)
divide line = splitAt ind line where ind = length line `div` 2

common :: String -> String -> String -> Char
common xs ys zs = head . Set.toList $ Set.intersection (Set.intersection a b) c 
  where a = Set.fromList xs
        b = Set.fromList ys
        c = Set.fromList zs


toPriority :: Char -> Int
toPriority x | x `elem` ['a'..'z'] = ord x - ord 'a' + 1
toPriority x | x `elem` ['A'..'Z'] = ord x - ord 'A' + 27

parseLines :: [String] -> Int
parseLines [] = 0
parseLines (x: y: z: xs) = toPriority (common x y z) + parseLines xs

solve :: String -> Int
solve file = parseLines $ lines file

main = do 
  fileString <- readFile "input.txt"
  let solution = solve fileString
  print solution

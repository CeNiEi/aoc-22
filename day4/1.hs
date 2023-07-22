import Data.List (elemIndex)

mySplit :: Char -> String -> Maybe (String, String)
mySplit x xs = do
  index <- elemIndex x xs
  let (a, b) = splitAt index xs
  return $ (a, tail b)

parseLine :: String -> Maybe (Maybe (Int, Int), Maybe (Int, Int))
parseLine line =
  mySplit ',' line >>= \(xs, ys) -> return ((mySplit '-' xs) >>= \(a, b) -> return (read a, read b), (mySplit '-' ys) >>= \(a, b) -> return (read a, read b))

solveHelper :: ((Int, Int), (Int, Int)) -> Bool
solveHelper (x, y) = (fst x <= fst y && snd x >= snd y) || (fst y <= fst x && snd y >= snd x) 

solve :: String -> [Maybe Bool]
solve file = map ( (>>= \(a, b) -> (solveRange (a, b))) . parseLine) $ lines file
  where solveRange (Just a, Just b) = Just $ solveHelper (a, b)
        solveRange _ = Nothing

count :: [Maybe Bool] -> Maybe Int
count [Nothing] = Nothing
count [Just a] = Just (if a then 1 else 0)
count (x: xs) = count xs >>= (\a -> case x of 
  Just b -> Just ((if b then 1 else 0) + a)
  _ -> Nothing
  )

main = do
  file <- readFile "input.txt"
  let solution = count . solve $ file
  print solution

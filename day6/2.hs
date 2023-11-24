valid :: String -> Bool
valid [a] = True 
valid (x: xs) = (length $ filter (==x) xs) /= 1 && valid xs

parseStream :: String -> Maybe Int
parsStream xs | length xs < 14 = Nothing
              | length xs == 14 && valid xs = Just 14
parseStream (x: xs) = if valid (x : take 13 xs) then Just 14 else parseStream xs >>= \x -> Just $ x + 1

main = do
  input <- readFile "input.txt"
  print $ parseStream input

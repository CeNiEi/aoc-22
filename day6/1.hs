parseStream :: String -> Maybe Int
parseStream [] = Nothing 
parseStream [a, b] = Nothing
parseStream [a, b, c] = Nothing
parseStream [a, b, c, d] = if a /= b && a /= c && a /= d && b /= c && b /= d && c /= d
                             then Just 4 else Nothing
parseStream (x: xs) = if x /= xs!!0 && x /= xs!!1 && x /= xs!!2 && xs!!0 /= xs!!1 && xs!!0 /= xs!!2 && xs!!1 /= xs!!2 
                        then Just 4 else parseStream xs >>= \x -> Just $ x + 1

main = do
  input <- readFile "input.txt"
  print $ parseStream input

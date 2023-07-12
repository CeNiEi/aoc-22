data Hand = Rock | Paper | Scissor deriving Eq

type Round = (Hand, Hand)

parseToHand :: Char -> Hand
parseToHand x
  | x `elem` "AX" = Rock
  | x `elem` "BY" = Paper
  | x `elem` "CZ" = Scissor

handToScore :: Hand -> Int
handToScore Rock = 1
handToScore Paper = 2
handToScore Scissor = 3

parseToRound :: String -> Round
parseToRound [x, ' ', y] = (parseToHand x, parseToHand y)

roundToScore :: Round -> Int
roundToScore a 
  | a `elem` [(Scissor, Rock), (Paper, Scissor), (Rock, Paper)] = 6
  | a `elem` [(Rock, Rock), (Paper, Paper), (Scissor, Scissor)] = 3
  | otherwise = 0

countScore :: Round -> Int
countScore (x, y) = roundToScore (x, y) + handToScore y


solve :: String -> Int
solve file = foldl (+) 0 $ map (countScore . parseToRound) $ lines file


main = do
  fileString <- readFile "input.txt"
  let solution = solve fileString
  print solution

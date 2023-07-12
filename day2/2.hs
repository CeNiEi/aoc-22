import Text.Parsec (parse)
data Hand = Rock | Paper | Scissor deriving Eq
data End = Win | Lose | Draw deriving Eq

type Round = (Hand, End)

parseToHand :: Char -> Hand
parseToHand 'A' = Rock
parseToHand 'B' = Paper
parseToHand 'C' = Scissor

parseToEnd :: Char -> End
parseToEnd 'X' = Lose
parseToEnd 'Y' = Draw 
parseToEnd 'Z' = Win 
  
handToScore :: Hand -> Int
handToScore Rock = 1
handToScore Paper = 2
handToScore Scissor = 3

myHand :: Round -> Hand
myHand (a, Draw) = a
myHand (a, Win) = case a of
                Rock -> Paper
                Scissor -> Rock
                Paper -> Scissor
myHand (a, Lose) = case a of
                Rock -> Scissor
                Scissor -> Paper
                Paper -> Rock

parseToRound :: String -> Round
parseToRound [x, ' ', y] = (parseToHand x, parseToEnd y)

roundToScore :: Round -> Int
roundToScore (a, b) = (handToScore . myHand) (a, b) + case b of
                                                      Win -> 6
                                                      Draw -> 3
                                                      Lose -> 0

solve :: String -> Int
solve file = foldl (+) 0 $ map (roundToScore . parseToRound) $ lines file


main = do
  fileString <- readFile "input.txt"
  let solution = solve fileString
  print solution

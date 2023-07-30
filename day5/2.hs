import Data.Char (isAsciiUpper)
import Data.List (findIndex)

type Command = (Int, (Int, Int))
type Stacks = [String]

parseCommand :: String -> Command
parseCommand line = (read (j !! 1), (read (j !! 3) - 1, read (j !! 5) - 1)) where j = words line

parseLayout :: String -> [Maybe Char]
parseLayout line = [if isAsciiUpper (line !! j)
                    then Just (line !! j)
                    else Nothing | j <- [1,5..(length line - 1)]]

splitLayoutFromCommands :: [String] -> ([String], [String])
splitLayoutFromCommands lines = (a, tail b) where
    (a, b) = splitAt index lines
    index = case (findIndex (\x -> x !! 1 == '1') lines) of
              Just a -> a

addToStack :: [String] -> [Maybe Char]-> [String]
addToStack allRows row = [case row !! j of
                            Just x -> x : (allRows !! j)
                            Nothing -> (allRows !! j) | j <- [0..(length row - 1)]]


makeStacks :: [[Maybe Char]] -> Stacks 
makeStacks layout = foldl addToStack init layout where init = [[] | _ <- (layout !! 0)]

moveN :: Stacks -> Command -> Stacks 
moveN stacks (num, (from, to)) = [if j == from then drop num (stacks !! j)
                                    else if j == to then moved ++ (stacks !! j) 
                                    else stacks !! j | j <- [0..(length stacks - 1)]]
                                      where moved = take num $ stacks !! from


main = do
  file <- readFile "input.txt"
  let (layout, commands) = splitLayoutFromCommands $ lines file

  let stacks = map reverse $ makeStacks $ map parseLayout layout
  let parsedCommands = map parseCommand (filter (not . null) commands)

  let shuffledStacks = foldl moveN stacks parsedCommands

  let answer = map head shuffledStacks

  print answer 

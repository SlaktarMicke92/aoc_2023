# Solutions

### Part 1

```haskell
import Data.List (isPrefixOf)

readData :: FilePath -> IO String
readData file = do
  readFile file

redCubeLimit :: Int
redCubeLimit = 12

greenCubeLimit :: Int
greenCubeLimit = 13

blueCubeLimit :: Int
blueCubeLimit = 14

first :: [String] -> [String]
first [] = []
first (x:xs) = x:second xs

second :: [String] -> [String]
second [] = []
second (_:xs) = first xs

getColorValues :: String -> (String, String) -> Int
getColorValues checkColor (value, color) = do
    if checkColor `isPrefixOf` color then read value :: Int else 0

isGamePossible :: Int -> Int -> Int -> Bool
isGamePossible red green blue = red <= redCubeLimit && green <= greenCubeLimit && blue <= blueCubeLimit

findCubes :: [String] -> String -> Int
findCubes splitLines checkColor = do
    let zipped = zip (first splitLines) (second splitLines)
    let cubeList = map (getColorValues checkColor) zipped
    if null cubeList then 0 else maximum cubeList 

getIfPossible :: String -> Int
getIfPossible line = do
    let drop_game = drop 1 $ words line
    let gameId = read $ filter (not . (`elem` ":")) $ head drop_game
    let drop_id = drop 1 drop_game
    let red = findCubes drop_id "red"
    let green = findCubes drop_id "green"
    let blue = findCubes drop_id "blue"
    let possible = isGamePossible red green blue
    if possible then gameId else 0

main :: IO ()
main = do
    content <- readData "input.txt"
    print $ sum $ map getIfPossible $ lines content
```

### Part 2

```haskell
import Data.List (isPrefixOf)

readData :: FilePath -> IO String
readData file = do
  readFile file

first :: [String] -> [String]
first [] = []
first (x:xs) = x:second xs

second :: [String] -> [String]
second [] = []
second (_:xs) = first xs

getColorValues :: String -> (String, String) -> Int
getColorValues checkColor (value, color) = do
    if checkColor `isPrefixOf` color then read value :: Int else 0

powerOfSet :: Int -> Int -> Int -> Int
powerOfSet red green blue = red * green * blue

findCubes :: [String] -> String -> Int
findCubes splitLines checkColor = do
    let zipped = zip (first splitLines) (second splitLines)
    let cubeList = map (getColorValues checkColor) zipped
    if null cubeList then 0 else maximum cubeList 

getIfPossible :: String -> Int
getIfPossible line = do
    let game = words line
    let red = findCubes game "red"
    let green = findCubes game "green"
    let blue = findCubes game "blue"
    powerOfSet red green blue

main :: IO ()
main = do
    content <- readData "input.txt"
    print $ sum $ map getIfPossible $ lines content
```
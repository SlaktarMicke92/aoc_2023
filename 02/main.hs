
readData :: FilePath -> IO String
readData file = do
  readFile file

redCubeLimit :: Int
redCubeLimit = 12

greenCubeLimit :: Int
greenCubeLimit = 13

blueCubeLimit :: Int
blueCubeLimit = 14

isGamePossible :: Int -> Int -> Int -> Int
isGamePossible reds greens blues = if reds <= redCubeLimit && greens <= greenCubeLimit && blues <= blueCubeLimit then 1 else 0

findCubes :: String -> String -> Int
findCubes line color = do
    -- Fix logic for this function
    1

getIfPossible :: String -> Int
getIfPossible line = do
    let red = findCubes line "red"
    let green = findCubes line "green"
    let blue = findCubes line "blue"
    isGamePossible red green blue

main :: IO ()
main = do
    content <- readData "input.txt"
    print $ sum $ map getIfPossible $ lines content
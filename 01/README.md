# Solution

### Part 1

```haskell
import Data.Char (isDigit)

readData :: FilePath -> IO String
readData file = do
  readFile file

headDef :: String -> Int
headDef [] = 0
headDef (x:_) = read [x]

getNumber :: String -> Int
getNumber line = headDef $ filter isDigit line

getSum :: String -> Int
getSum content
  = sum
  $ map processLine
  $ lines content
  where
    processLine :: String -> Int
    processLine line = do
      let firstNumber = getNumber line
      let secondNumber = getNumber $ reverse line
      (firstNumber * 10) + secondNumber

main :: IO ()
main = do
  content <- readData "input.txt"
  let outputData = getSum content
  print outputData

```

### Part 2

```haskell

```
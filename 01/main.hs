import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf, tails)
import Data.Maybe

readData :: FilePath -> IO String
readData file = do
  readFile file

getNumber :: String -> Maybe Int
getNumber [] = Nothing
getNumber line@(x:_)
  | "one" `isPrefixOf` line = Just 1
  | "two" `isPrefixOf` line = Just 2
  | "three" `isPrefixOf` line = Just 3
  | "four" `isPrefixOf` line = Just 4
  | "five" `isPrefixOf` line = Just 5
  | "six" `isPrefixOf` line = Just 6
  | "seven" `isPrefixOf` line = Just 7
  | "eight" `isPrefixOf` line = Just 8
  | "nine" `isPrefixOf` line = Just 9
  | isDigit x = Just $ digitToInt x
  | otherwise = Nothing

firstDigit :: (String -> Maybe Int) -> String -> Int
firstDigit maybeDigit line = head digits
  where
    digits = mapMaybe maybeDigit $ tails line


lastDigit :: (String -> Maybe Int) -> String -> Int
lastDigit maybeDigit line = last digits
  where
    digits = mapMaybe maybeDigit $ tails line

getSum :: String -> Int
getSum content
  = sum
  $ Prelude.map processLine
  $ Prelude.lines content
  where
    processLine :: String -> Int
    processLine line = do
      let firstNumber =  firstDigit getNumber line
      let secondNumber = lastDigit getNumber line
      (firstNumber * 10) + secondNumber

main :: IO ()
main = do
  content <- readData "input.txt"
  let outputData = getSum content
  print outputData

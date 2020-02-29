import System.Environment
import Text.Read

lowers = ['a'..'z']
uppers = ['A'..'Z']


rotate :: Int -> Char -> Char
(rotate y) x
  | (inside lowers) = (toEnum ((((fromEnum x)-(fromEnum 'a')+y) `mod` 26)+(fromEnum 'a'))::Char)
  | (inside uppers) = (toEnum ((((fromEnum x)-(fromEnum 'A')+y) `mod` 26)+(fromEnum 'A'))::Char)
  | otherwise = x
    where inside = (elem x)


findLetterPos :: Char -> Int
findLetterPos x
  | (inside lowers) = (fromEnum x)-(fromEnum 'a')
  | (inside uppers) = (fromEnum x)-(fromEnum 'A')
  | otherwise = 0
    where inside = (elem x)


vigenere :: String -> String -> String
(vigenere shiftStr) shiftContents = zipWith rotate (cycle (map findLetterPos shiftStr)) shiftContents


main :: IO()
main = do
  a <- getArgs --get shift word from command line args
  ss <- getContents --get shift contents
  let b = (head a)
  putStr ((vigenere b) ss)

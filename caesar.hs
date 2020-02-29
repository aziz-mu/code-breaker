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

caesar :: Int -> String -> String
(caesar a) s = ((map (rotate a) s))

main :: IO()
main = do
  a <- getArgs --get shift number from command arguments
  ss <- getContents --get shift contents
  let b = (readMaybe (head a)) :: Maybe Int
  case b of
    Just n -> putStr ((caesar n) ss)
    Nothing -> putStr "Usage: Caesar [shift number]"

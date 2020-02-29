lowers = ['a'..'z']
uppers = ['A'..'Z']

rotate::Char->Char
rotate x
  | (inside lowers) = (toEnum((((fromEnum x)-(fromEnum 'a')+13) `mod` 26)+(fromEnum 'a'))::Char)
  | (inside uppers) = (toEnum((((fromEnum x)-(fromEnum 'A')+13) `mod` 26)+(fromEnum 'A'))::Char)
  | otherwise = x
  where inside = (elem x)

rot13::String -> String
rot13 s = ((map rotate) s)

main :: IO()
main = do
  ss <- getContents
  putStr (rot13 ss)


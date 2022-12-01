splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy delim lst =
  let (x, xs) = span (/= delim) lst
  in x : splitBy delim (drop 1 xs)
  
main = interact $ show . foldl1 max . map sum . map (map readInt) . splitBy "" . lines
  where readInt :: String -> Int
        readInt = read

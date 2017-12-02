import Data.Char

stringToIntegerList :: String -> [Int]
stringToIntegerList string = [digitToInt element | element <- string]

match :: FilePath -> IO Int
match path = do
      content <- readFile path
      let a = [digitToInt element | element <- content]
      return (sumMatch a)

sumMatch :: [Int] -> Int
sumMatch list = sum [if (i == (length list)-1) then (if (last list) == (head list) then (head list) else 0) else (if(list !! (i+1) == element) then element else 0) | (i,element) <- zip [0..] list]

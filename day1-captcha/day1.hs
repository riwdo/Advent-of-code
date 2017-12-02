import Data.Char

stringToIntegerList :: String -> [Int]
stringToIntegerList string = [digitToInt element | element <- string]

makeList :: IO ([Int])
makeList = do
      content <- readFile "Input.txt"
      let a = [digitToInt element | element <- content]
      let b = match a
      return a

match :: IO (Int) -> IO Int
match list = sum [if (i == (length list)-1) then (if (last list) == (head list) then (head list) else 0) else (if(list !! (i+1) == element) then element else 0) | (i,element) <- zip [0..] list]


--main :: IO ()
--main =  c = makeList "input.txt"

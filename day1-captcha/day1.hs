import Data.Char


-- A
stringToIntegerList :: String -> [Int]
stringToIntegerList string = [digitToInt element | element <- string]

match :: FilePath -> IO Int
match path = do
      content <- readFile path
      let a = [digitToInt element | element <- content]
      return (sumMatchXSteps a)

sumMatch :: [Int] -> Int
sumMatch list = sum [if (i == (length list)-1) then (if (last list) == (head list) then (head list) else 0) else (if(list !! (i+1) == element) then element else 0) | (i,element) <- zip [0..] list]

--B

sumMatchXSteps :: [Int] -> Int
sumMatchXSteps list = sum [if (i == (l)-1) then (if ((list !! ((i+stepSize) - l) == element)) then (element) else 0) else (if (i+stepSize >= l) then (if (list !! ((i+stepSize) - l) == element) then element else 0)  else (if(list !! (i+stepSize) == element) then element else 0)) | (i,element) <- zip [0..] list]
                      where (l,stepSize) = (length list, fromIntegral l `div` fromIntegral 2)

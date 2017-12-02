readInput :: FilePath -> IO Int
readInput path = do
      content <- readFile path
      let matrix = parseMatrix content :: [[Int]]
      let sumation = sum [difference list | list <- matrix]
      return sumation

parseMatrix :: (Read a) => String -> [[a]]
parseMatrix s =  (map (map read . words) $ lines s)

difference :: [Int] -> Int
difference list = maximum list - minimum list

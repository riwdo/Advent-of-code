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


--B

readInputEven :: FilePath -> IO Int
readInputEven path = do
      content <- readFile path
      let matrix = parseMatrix content :: [[Int]]
      let sumation = sum [addDivEven list | list <- matrix]
      return sumation

addDivEven :: [Int] -> Int
addDivEven [] = 0
addDivEven (x:list) = sum [checkMod x element | element <- (list)] + addDivEven list
                                    where checkMod n1 n2 = if(n1 `mod` n2 == 0) then n1 `div` n2 else (if(n2 `mod` n1 == 0) then n2 `div` n1 else 0)

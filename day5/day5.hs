readInput :: FilePath -> IO Int
readInput path = do
      content <- readFile path
      let list = parseStringArray (lines content)
      return (findExit list 0 0 (length list) (head list))

parseStringArray :: [String] -> [Int]
parseStringArray list = [read element :: Int | element <- list]

findExit :: [Int] -> Int -> Int -> Int -> Int -> Int
findExit list i steps l stepSize | (i+stepSize) < 0  || (i+stepSize) >= (l) = steps
                                 | otherwise = findExit (take i list ++ [stepSize+1] ++ drop (i+1) list) (i+stepSize) (steps+1) l (list !! (i+stepSize))

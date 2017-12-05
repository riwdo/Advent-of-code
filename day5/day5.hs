readInput :: FilePath -> IO Int
readInput path = do
      content <- readFile path
      let list = parseStringArray (lines content)
      --return list
      return (findExit list 0 0 (head list))

parseStringArray :: [String] -> [Int]
parseStringArray list = [read element :: Int | element <- list]

findExit :: [Int] -> Int -> Int -> Int -> Int
findExit list i steps stepSize | (i) < (length list) && (i) >= (-1) = findExit updatedList (newIndex) (steps+1) (updatedList !! (newIndex))
                               | otherwise = steps
                               where (updatedList,newIndex) = (take i list ++ [stepSize+1] ++ drop (i+1) list, (i+stepSize))

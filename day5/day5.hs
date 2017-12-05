import Data.Sequence
import Control.DeepSeq

readInput :: FilePath -> IO Int
readInput path = do
      content <- readFile path
      let list = parseStringArray (lines content)
      --return list
      let sequenc = fromList list
      return (findExitB sequenc 0 0 (head list) (Data.Sequence.length sequenc))


parseStringArray :: [String] -> [Int]
parseStringArray list = [read element :: Int | element <- list]

-- A

findExit :: Seq Int -> Int -> Int -> Int -> Int -> Int
findExit list i steps stepSize l | (i) < (l) && (i) >= (-1) = findExit  updatedList (newIndex) (steps+1) (index updatedList newIndex) l
                               | otherwise = steps
                               where (updatedList,newIndex) = ((update i (stepSize+1) $ list), (i+stepSize))

-- B

findExitB :: Seq Int -> Int -> Int -> Int -> Int -> Int
findExitB list i steps stepSize l | (i) < (l) && (i) >= (-1) = findExitB updatedList (newIndex) (deepseq steps (steps+1)) (index updatedList (newIndex)) l
                                  | otherwise = steps
                                where (updatedList,newIndex,incrementCount) = (deepseq list (update i ((stepSize +incrementCount)) $ list ),(i+stepSize),(if stepSize >= 3 then -1 else 1))

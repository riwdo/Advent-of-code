import Data.List.Split
import Data.List

-- A
readInput :: FilePath -> IO Int
readInput input = do
              list <- readFile input
              return $ validPassphrases $ lines list

validPassphrases :: [String] -> Int
validPassphrases listOfPass = sum [if (checkDuplicate (splitOn " " passphrase) == True) then 0 else 1 | passphrase <- listOfPass]
                              where checkDuplicate (x:wordList) | elem x wordList = True
                                                                | wordList == [] = False
                                                                | otherwise = checkDuplicate wordList
-- B
readInputAna :: FilePath -> IO Int
readInputAna input = do
              list <- readFile input
              return $ validPassphrasesAna $ splitAndSort $ lines list
              --return $ sortListElem $ lines list
              --return $ validPassphrases (sortListElem $ lines list)

splitAndSort :: [String] -> [String]
splitAndSort list = [sortListElements (splitOn " " (x)) | x <- list]

sortListElements :: [String] -> String
sortListElements list = intercalate " " [sort element | element <- list]

validPassphrasesAna :: [String] -> Int
validPassphrasesAna listOfPass = sum [if (checkDuplicate (splitOn " " passphrase) == True) then 0 else 1 | passphrase <- listOfPass]
                              where checkDuplicate (x:wordList) | elem x wordList = True
                                                                | wordList == [] = False
                                                                | otherwise = checkDuplicate wordList

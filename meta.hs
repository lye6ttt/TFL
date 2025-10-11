import System.Random
import Control.Monad (replicateM)
import Data.List (find, isPrefixOf, tails)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

type Rule = (String, String)

srsT :: [Rule]
srsT = [
    ("cb", "ba"), ("aaa", "aa"), ("aba", "ba"), 
    ("ac", "cc"), ("baa", "ba"), ("bbb", "b"), 
    ("bbc", "c"), ("ba", "cab"), ("cac", "cc"), 
    ("bab", "cac"), ("ccc", "c"), ("babb", "ba"), 
    ("babc", "")
    ]

srsT' :: [Rule]
srsT' = [("a", ""), ("bb", ""), ("c", "")]

alphabet :: String
alphabet = "abc"

-- генерируем случайную строку
generateRandomString :: Int -> Int -> IO String
generateRandomString minLen maxLen = do
    len <- randomRIO (minLen, maxLen)
    replicateM len $ do
        idx <- randomRIO (0, length alphabet - 1)
        return $ alphabet !! idx

-- находим все вхождения подстроки в строку
findAllOccurrences :: String -> String -> [Int]
findAllOccurrences pattern text =
    [i | (i, tail) <- zip [0..] (tails text), pattern `isPrefixOf` tail]

-- заменяем подстроку по индексу
replaceSubstring :: Int -> Int -> String -> String -> String
replaceSubstring idx len replacement str =
    let (before, after) = splitAt idx str
    in before ++ replacement ++ drop len after

-- случайным образом преобразовываем строку по srs
randomlyTransform :: String -> [Rule] -> IO String
randomlyTransform inputStr srs = do
    numApplications <- randomRIO (5, 10)
    go numApplications inputStr
  where
    go :: Int -> String -> IO String
    go 0 s = return s
    go n s = do
        let possibleChanges = do
                (pattern, replacement) <- srs
                idx <- findAllOccurrences pattern s
                return (idx, pattern, replacement)
        
        if null possibleChanges
        then return s
        else do
            (idx, pattern, replacement) <- (possibleChanges !!) <$> randomRIO (0, length possibleChanges - 1)
            let newStr = replaceSubstring idx (length pattern) replacement s
            go (n - 1) newStr

-- подсчет количества букв "b" в строке
countB :: String -> Int
countB = length . filter (== 'b')

-- проверяем инвариант1: невозрастание количества букв b
checkInvariant1 :: String -> String -> String -> IO Bool
checkInvariant1 w w' w'' = 
    if (countB w >= countB w')
    then return (countB w >= countB w'')
    else return False

-- проверяем инвариант2: сохранение четности количества букв b
checkInvariant2 :: String -> String -> String -> IO Bool
checkInvariant2 w w' w'' = 
    if (countB w >= countB w')
    then return (countB w >= countB w'')
    else return False

main :: IO ()
main = do
    w <- generateRandomString 10 20
    putStrLn $ "w: " ++ w

    w' <- randomlyTransform w srsT
    putStrLn $ "w': " ++ w'

    w'' <- randomlyTransform w srsT'
    putStrLn $ "w'': " ++ w''

    invariant1 <- checkInvariant1 w w' w''
    putStrLn $ "invariant1: " ++ show invariant1

    invariant2 <- checkInvariant2 w w' w''
    putStrLn $ "invariant2: " ++ show invariant2
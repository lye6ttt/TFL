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

-- сравнение по military порядку
compareLengthLex :: String -> String -> Ordering
compareLengthLex s1 s2
    | length s1 < length s2 = LT
    | length s1 > length s2 = GT
    | otherwise             = compare s1 s2

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

-- случайным образом преобразовываем строку по srs T
randomlyTransform :: String -> IO String
randomlyTransform inputStr = do
    numApplications <- randomRIO (5, 10)
    go numApplications inputStr
  where
    go :: Int -> String -> IO String
    go 0 s = return s
    go n s = do
        let possibleChanges = do
                (pattern, replacement) <- srsT
                idx <- findAllOccurrences pattern s
                return (idx, pattern, replacement)
        
        if null possibleChanges
        then return s
        else do
            (idx, pattern, replacement) <- (possibleChanges !!) <$> randomRIO (0, length possibleChanges - 1)
            let newStr = replaceSubstring idx (length pattern) replacement s
            go (n - 1) newStr

-- всевозможные слова, достижимые за один шаг
getNextStates :: String -> [Rule] -> Set.Set String
getNextStates str rules = Set.fromList $ do
    (pattern, replacement) <- rules
    let indices = findAllOccurrences pattern str
    idx <- indices
    return $ replaceSubstring idx (length pattern) replacement str

-- смотрим все достижимые слова из startStr
findAllReachable :: String -> [Rule] -> Set.Set String
findAllReachable startStr rules = bfs (Set.singleton startStr) (Seq.singleton startStr)
  where
    bfs visited queue
        | Seq.null queue = visited
        | otherwise =
            let (current Seq.:<| rest) = queue
                newStrings = getNextStates current rules
                unvisited = Set.difference newStrings visited
                newVisited = Set.union visited unvisited
                newQueue = rest Seq.>< Seq.fromList (Set.toList unvisited)
            in bfs newVisited newQueue

-- проверяем, можно ли startStr преобразовать к строке из reachableFromShorter
checkIntersection :: String -> [Rule] -> Set.Set String -> Bool
checkIntersection startStr rules targetSet
    | startStr `Set.member` targetSet = True
    | otherwise = bfs (Set.singleton startStr) (Seq.singleton startStr)
  where
    bfs visited queue
        | Seq.null queue = False
        | otherwise =
            let (current Seq.:<| rest) = queue
                newStrings = getNextStates current rules
            in case find (`Set.member` targetSet) (Set.toList newStrings) of
                Just _ -> True
                Nothing ->
                    let unvisited = Set.difference newStrings visited
                        newVisited = Set.union visited unvisited
                        newQueue = rest Seq.>< Seq.fromList (Set.toList unvisited)
                    in bfs newVisited newQueue

main :: IO ()
main = do
    w <- generateRandomString 10 20
    putStrLn $ "w: " ++ w

    w' <- randomlyTransform w
    putStrLn $ "w': " ++ w'

    let (shorterStr, longerStr) =
            if compareLengthLex w w' /= GT
            then (w, w')
            else (w', w)

    let reachableFromShorter = findAllReachable shorterStr srsT'
    let areEquivalent = checkIntersection longerStr srsT' reachableFromShorter

    putStrLn $ "are equivalent? " ++ show areEquivalent
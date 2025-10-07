import System.Random
import Control.Monad (replicateM)
import Data.List (find, isPrefixOf, tails)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

type Rule = (String, String)

srsT :: [Rule]
srsT = [
    ("cb", "ba"), ("aaa", "aa"), ("aba", "ba"), ("ac", "cc"),
    ("baa", "ba"), ("bba", "ba"), ("bbb", "b"), ("bbc", "c"),
    ("bcc", "cc"), ("ba", "cab"), ("cac", "cc"), ("bab", "cac"),
    ("ccc", "c"), ("babb", "ba"), ("babc", ""), ("baca", "cabba"),
    ("caab", "bb"), ("caac", "bc"), ("aabcaa", "a")
    ]

srsT' :: [Rule]
srsT' = [("a", ""), ("b", ""), ("c", "")]

alf :: String
alf = "abc"

generateRandomString :: Int -> Int -> IO String
generateRandomString minLen maxLen = do
    len <- randomRIO (minLen, maxLen)
    replicateM len $ do
        idx <- randomRIO (0, length alf - 1)
        return $ alf !! idx

findAllOccurrences :: String -> String -> [Int]
findAllOccurrences pattern text =
    [i | (i, tail) <- zip [0..] (tails text), pattern `isPrefixOf` tail]

replaceSubstring :: Int -> Int -> String -> String -> String
replaceSubstring pos oldLen newText str =
    let (before, after) = splitAt pos str
    in before ++ newText ++ drop oldLen after

getNextStates :: String -> [Rule] -> Set.Set String
getNextStates current rules = Set.fromList $ do
    (pattern, replacement) <- rules
    positions <- findAllOccurrences pattern current
    let newStr = replaceSubstring positions (length pattern) replacement current

    if any (`isPrefixOf` current) (tails pattern)
    then return newStr
    else return newStr

randomlyTransform :: String -> IO String
randomlyTransform input = do
    steps <- randomRIO (5, 10)
    let initialState = ([], input, steps)
    (_, result, _) <- transformLoop initialState
    return result
  where
    transformLoop :: ([String], String, Int) -> IO ([String], String, Int)
    transformLoop (history, current, stepsLeft)
        | stepsLeft <= 0 = return (history, current, stepsLeft)
        | otherwise = do
            let possibleChanges = [(pos, pat, rep) | 
                                  (pat, rep) <- srsT, 
                                  pos <- findAllOccurrences pat current]
            
            if null possibleChanges
            then return (history, current, stepsLeft)
            else do
                index <- randomRIO (0, length possibleChanges - 1)
                let (pos, pattern, replacement) = possibleChanges !! index
                let nextStr = replaceSubstring pos (length pattern) replacement current
                
                let newHistory = history ++ [current]  
                transformLoop (newHistory, nextStr, stepsLeft - 1)

findAllReachable :: String -> [Rule] -> Set.Set String
findAllReachable start rules = bfs (Set.singleton start) (Seq.singleton start)
  where
    bfs visited queue
        | Seq.null queue = visited
        | otherwise =
            let (current Seq.:<| rest) = queue
                nextStates = getNextStates current rules
                unvisited = Set.filter (`Set.notMember` visited) nextStates
                newVisited = Set.union visited unvisited
                newQueue = rest Seq.>< Seq.fromList (Set.toList unvisited)
            in bfs newVisited newQueue

check :: String -> [Rule] -> Set.Set String -> Bool
check startStr rules strings
    | startStr `Set.member` strings = True
    | otherwise = bfs (Set.singleton startStr) (Seq.singleton startStr)
  where
    bfs visited queue
        | Seq.null queue = False
        | otherwise =
            let (current Seq.:<| rest) = queue
                newStrings = getNextStates current rules
            in case find (`Set.member` strings) (Set.toList newStrings) of
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

    let (shorter, longer) =
            if length w <= length w'
            then (w, w')
            else (w', w)

    let areEquivalent = check longer srsT (findAllReachable shorter srsT')

    putStrLn $ show areEquivalent
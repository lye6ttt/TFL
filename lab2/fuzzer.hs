import System.Random
import System.IO
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Array
import Text.Regex.TDFA
import Data.Maybe (fromMaybe)

---------------------------------------------------------------------
-- случайные генераторы
---------------------------------------------------------------------
randomChoice :: [a] -> IO a
randomChoice xs = do
    index <- randomRIO (0, length xs - 1)
    return (xs !! index)

generateRandomWord :: Int -> String -> IO String
generateRandomWord n alphabet = replicateM n $ do
    i <- randomRIO (0, length alphabet - 1)
    return (alphabet !! i)

generateWord :: Int -> IO String
generateWord maxLength = do
    length <- randomRIO (1, maxLength)
    generateRandomWord length "abc"

---------------------------------------------------------------------
-- регулярка
---------------------------------------------------------------------
generateByRegex :: IO String
generateByRegex = do
    n1 <- randomRIO (0, 5)
    part1 <- concat <$> replicateM n1 (randomChoice ["aa", "bb", "cc"])
    
    n2 <- randomRIO (0, 5)
    part2 <- concat <$> replicateM n2 (randomChoice ["aaa", "bbb"])
    
    n3 <- randomRIO (0, 5)
    part3 <- concat <$> replicateM n3 generateInnerPattern
    
    lastChar <- randomChoice ["a", "b", "c"]
    addB <- randomRIO (0, 1 :: Int)
    
    return $ part1 ++ "b" ++ part2 ++ part3 ++ "abc" ++ lastChar ++ if addB == 1 then "b" else ""
  where
    generateInnerPattern :: IO String
    generateInnerPattern = do
        n <- randomRIO (0, 5)
        inner <- concat <$> replicateM n (randomChoice ["ab", "bc", "ccc"])
        return $ inner ++ "aa"

isAcceptedByRegex :: String -> Bool
isAcceptedByRegex w = w =~ ("^(aa|bb|cc)*b(aaa|bbb)*((ab|bc|ccc)*aa)*abc(a|b|c)(b|())$" :: String)

---------------------------------------------------------------------
-- ДКА
---------------------------------------------------------------------
data DFAConfig = DFAConfig
    { startState :: Int
    , acceptStates :: S.Set Int
    , stepFunction :: Int -> Char -> Int
    , trapState :: Int
    }

generateByDFA :: DFAConfig -> Int -> IO String
generateByDFA config maxLength = generatePath (startState config) ""
  where
    generatePath current path
        | length path >= maxLength = 
            if current `S.member` acceptStates config
                then return path 
                else generateByDFA config maxLength
        | current `S.member` acceptStates config = do
            stop <- randomRIO (0, 1 :: Int)
            if stop == 0 || length path >= maxLength
                then return path
                else continueGeneration
        | otherwise = continueGeneration
      where
        continueGeneration = do
            let possibleTransitions = 
                    [ (c, next) | c <- "abc", 
                      let next = stepFunction config current c, 
                      next /= trapState config ]
            
            if null possibleTransitions
                then generateByDFA config maxLength
                else do
                    (char, nextState) <- randomChoice possibleTransitions
                    generatePath nextState (path ++ [char])

trap :: Int
trap = -1

dfa :: [(Int, Char, Int)]
dfa =
  [ (0,'a',15),(0,'b',16),(0,'c',17)
  , (1,'a',trap),(1,'b',trap),(1,'c',2)
  , (2,'a',21),(2,'b',1),(2,'c',20)
  , (3,'a',trap),(3,'b',26),(3,'c',trap)
  , (4,'a',21),(4,'b',1),(4,'c',27)
  , (5,'a',21),(5,'b',1),(5,'c',34)
  , (6,'a',21),(6,'b',1),(6,'c',39)
  , (7,'a',38),(7,'b',8),(7,'c',6)
  , (8,'a',19),(8,'b',7),(8,'c',5)
  , (9,'a',trap),(9,'b',26),(9,'c',2)
  , (10,'a',19),(10,'b',11),(10,'c',48)
  , (11,'a',15),(11,'b',12),(11,'c',4)
  , (12,'a',19),(12,'b',7),(12,'c',20)
  , (13,'a',45),(13,'b',46),(13,'c',20)
  , (14,'a',61),(14,'b',46),(14,'c',20)
  , (15,'a',0),(15,'b',trap),(15,'c',trap)
  , (16,'a',19),(16,'b',11),(16,'c',20)
  , (17,'a',trap),(17,'b',trap),(17,'c',0)
  , (trap,'a',trap),(trap,'b',trap),(trap,'c',trap)
  , (19,'a',60),(19,'b',24),(19,'c',trap)
  , (20,'a',trap),(20,'b',trap),(20,'c',1)
  , (21,'a',22),(21,'b',2),(21,'c',trap)
  , (22,'a',23),(22,'b',1),(22,'c',20)
  , (23,'a',22),(23,'b',24),(23,'c',trap)
  , (24,'a',21),(24,'b',1),(24,'c',25)
  , (25,'a',3),(25,'b',3),(25,'c',9)
  , (26,'a',trap),(26,'b',trap),(26,'c',trap)
  , (27,'a',15),(27,'b',16),(27,'c',28)
  , (28,'a',trap),(28,'b',trap),(28,'c',29)
  , (29,'a',30),(29,'b',31),(29,'c',32)
  , (30,'a',36),(30,'b',2),(30,'c',trap)
  , (31,'a',19),(31,'b',11),(31,'c',5)
  , (32,'a',trap),(32,'b',trap),(32,'c',33)
  , (33,'a',15),(33,'b',16),(33,'c',4)
  , (34,'a',trap),(34,'b',trap),(34,'c',35)
  , (35,'a',21),(35,'b',1),(35,'c',5)
  , (36,'a',37),(36,'b',31),(36,'c',32)
  , (37,'a',36),(37,'b',24),(37,'c',trap)
  , (38,'a',44),(38,'b',24),(38,'c',trap)
  , (39,'a',15),(39,'b',16),(39,'c',40)
  , (40,'a',21),(40,'b',1),(40,'c',41)
  , (41,'a',30),(41,'b',31),(41,'c',42)
  , (42,'a',trap),(42,'b',trap),(42,'c',43)
  , (43,'a',30),(43,'b',31),(43,'c',6)
  , (44,'a',13),(44,'b',31),(44,'c',32)
  , (45,'a',55),(45,'b',10),(45,'c',32)
  , (46,'a',21),(46,'b',47),(46,'c',48)
  , (47,'a',trap),(47,'b',52),(47,'c',2)
  , (48,'a',49),(48,'b',9),(48,'c',50)
  , (49,'a',22),(49,'b',51),(49,'c',trap)
  , (50,'a',trap),(50,'b',26),(50,'c',35)
  , (51,'a',21),(51,'b',1),(51,'c',20)
  , (52,'a',19),(52,'b',53),(52,'c',20)
  , (53,'a',trap),(53,'b',54),(53,'c',2)
  , (54,'a',trap),(54,'b',52),(54,'c',trap)
  , (55,'a',56),(55,'b',57),(55,'c',20)
  , (56,'a',58),(56,'b',10),(56,'c',32)
  , (57,'a',21),(57,'b',1),(57,'c',48)
  , (58,'a',59),(58,'b',57),(58,'c',20)
  , (59,'a',13),(59,'b',10),(59,'c',32)
  , (60,'a',14),(60,'b',1),(60,'c',20)
  , (61,'a',62),(61,'b',57),(61,'c',20)
  , (62,'a',14),(62,'b',57),(62,'c',20)
  ]

dfaMap :: M.Map (Int,Char) Int
dfaMap = M.fromList [ ((s,c),t) | (s,c,t) <- dfa ]

stepDFA :: Int -> Char -> Int
stepDFA st c = M.findWithDefault trap (st,c) dfaMap

dfaConfig :: DFAConfig
dfaConfig = DFAConfig
    { startState = 0
    , acceptStates = S.fromList [3,9,26,49,50,51]
    , stepFunction = stepDFA
    , trapState = trap
    }

generateByDFA' :: Int -> IO String
generateByDFA' = generateByDFA dfaConfig

isAcceptedByDFA :: String -> Bool
isAcceptedByDFA w = final `S.member` acceptStates dfaConfig
  where final = foldl stepDFA (startState dfaConfig) w

---------------------------------------------------------------------
-- НКА
---------------------------------------------------------------------
data NFAConfig = NFAConfig
    { nfaStartStates :: S.Set Int
    , nfaAcceptStates :: S.Set Int
    , nfaStepFunction :: S.Set Int -> Char -> S.Set Int
    }

generateByNFA :: NFAConfig -> Int -> IO String
generateByNFA config maxLength = generatePath (nfaStartStates config) ""
  where
    generatePath currentStates path
        | length path >= maxLength = 
            if not (S.null (S.intersection currentStates (nfaAcceptStates config)))
                then return path 
                else generateByNFA config maxLength
        | not (S.null (S.intersection currentStates (nfaAcceptStates config))) = do
            stop <- randomRIO (0, 1 :: Int)
            if stop == 0 || length path >= maxLength
                then return path
                else continueGeneration
        | otherwise = continueGeneration
      where
        continueGeneration = do
            let allPossible = 
                    [ (c, nextState) | 
                      state <- S.toList currentStates,
                      (c, nextState) <- M.findWithDefault [] state nfaMap ]
            
            if null allPossible
                then generateByNFA config maxLength
                else do
                    let byChar = M.fromListWith (++) [ (c, [ns]) | (c, ns) <- allPossible ]
                    chosenChar <- randomChoice (M.keys byChar)
                    let nextStates = S.fromList (byChar M.! chosenChar)
                    generatePath nextStates (path ++ [chosenChar])

nfa :: [(Int, Char, Int)]
nfa =
  [ (0,'a',1),(0,'b',3),(0,'c',5),(0,'b',7)
  , (1,'a',0)
  , (3,'b',0)
  , (5,'c',0)
  , (7,'a',8),(7,'b',11),(7,'a',14),(7,'b',16),(7,'c',18)
  , (7,'a',21),(7,'a',23)
  , (8,'a',9),(9,'a',7)
  , (11,'b',12),(12,'b',7)
  , (14,'b',15),(15,'a',14),(15,'a',21),(15,'b',16),(15,'c',18)
  , (16,'c',15),(18,'c',16)
  , (21,'a',22),(22,'a',14),(22,'a',21),(22,'a',23),(22,'b',16),(22,'c',18)
  , (23,'b',24),(24,'c',25)
  , (25,'a',26),(25,'b',26),(25,'c',26)
  , (26,'b',29)
  ]

nfaMap :: M.Map Int [(Char, Int)]
nfaMap = M.fromListWith (++) [ (s, [(c,t)]) | (s,c,t) <- nfa ]

stepNFA :: S.Set Int -> Char -> S.Set Int
stepNFA states c =
    let step st = S.fromList $ map snd $ filter ((==c) . fst) $
                  M.findWithDefault [] st nfaMap
    in S.unions (map step $ S.toList states)

nfaConfig :: NFAConfig
nfaConfig = NFAConfig
    { nfaStartStates = S.singleton 0
    , nfaAcceptStates = S.fromList [26,29]
    , nfaStepFunction = stepNFA
    }

generateByNFA' :: Int -> IO String
generateByNFA' = generateByNFA nfaConfig

isAcceptedByNFA :: String -> Bool
isAcceptedByNFA w = not (S.null (S.intersection final (nfaAcceptStates nfaConfig)))
  where final = foldl stepNFA (nfaStartStates nfaConfig) w

---------------------------------------------------------------------
-- ПКА
---------------------------------------------------------------------
type StatePair = (Int, Int)

data AFAConfig = AFAConfig
    { afaStartState :: StatePair
    , afaAcceptStates :: S.Set StatePair
    , afaStepFunction :: StatePair -> Char -> S.Set StatePair
    }

generateByAFA :: AFAConfig -> Int -> IO String
generateByAFA config maxLength = generatePath (S.singleton (afaStartState config)) ""
  where
    generatePath currentStates path
        | length path >= maxLength = 
            if not (S.null (S.intersection currentStates (afaAcceptStates config)))
                then return path 
                else
                    generateByAFA config maxLength
        
        | not (S.null (S.intersection currentStates (afaAcceptStates config))) = do
            stop <- randomRIO (0, 1 :: Int)
            if stop == 0 || length path >= maxLength
                then return path
                else continueGeneration path
                
        | otherwise = continueGeneration path
      where
        continueGeneration currentPath = do
            let possibleTransitions = 
                    [ ((state, c), nextState)
                    | state <- S.toList currentStates
                    , c <- ['a', 'b', 'c']
                    , let nextStates = afaStepFunction config state c
                    , not (S.null nextStates)
                    , nextState <- S.toList nextStates
                    ]
            
            if null possibleTransitions
                then generateByAFA config maxLength
                else do
                    ((state, chosenChar), nextState) <- randomChoice possibleTransitions
                    generatePath (S.singleton nextState) (currentPath ++ [chosenChar])

afaCommonPart :: [(Int, Char, Int)]
afaCommonPart = 
  [ (0,'a',1),(0,'b',3),(0,'c',5),(0,'b',7)
  , (1,'a',0)
  , (3,'b',0)
  , (5,'c',0)
  ]

afa1 :: [(Int, Char, Int)]
afa1 = 
  [ (7,'a',7),(7,'b',7),(7,'a',14),(7,'b',16),(7,'c',18)
  , (7,'a',21),(7,'a',23)
  , (14,'b',15),(15,'a',14),(15,'a',21),(15,'b',16),(15,'c',18)
  , (16,'c',15),(18,'c',16)
  , (21,'a',22),(22,'a',14),(22,'a',21),(22,'a',23),(22,'b',16),(22,'c',18)
  , (23,'b',24),(24,'c',25)
  , (25,'a',26),(25,'b',26),(25,'c',26)
  , (26,'b',29)
  ]

afa2 :: [(Int, Char, Int)]
afa2 = 
  [ (7,'a',8),(7,'b',11),(7,'a',14),(7,'b',16),(7,'c',18)
  , (7,'a',21),(7,'a',23)
  , (8,'a',9),(9,'a',7)
  , (11,'b',12),(12,'b',7)
  , (14,'b',15),(15,'a',14),(15,'a',21),(15,'b',16),(15,'c',18)
  , (16,'c',15),(18,'c',16)
  , (21,'a',22),(22,'a',14),(22,'a',21),(22,'a',23),(22,'b',16),(22,'c',18)
  , (23,'b',24)
  , (24,'a',25),(24,'b',25),(24,'c',25)
  , (25,'a',26),(25,'b',26),(25,'c',26)
  , (26,'a',29),(26,'b',29),(26,'c',29)
  ]

afaMap :: [(Int, Char, Int)] -> M.Map Int [(Char, Int)]
afaMap afaPart = M.fromListWith (++) [(s, [(c,t)]) | (s,c,t) <- afaPart]

stepSingle :: M.Map Int [(Char, Int)] -> Int -> Char -> S.Set Int
stepSingle transMap state char = 
    S.fromList [t | (c, t) <- M.findWithDefault [] state transMap, c == char]

stepPair :: StatePair -> Char -> S.Set StatePair
stepPair (s1, s2) c
    | s1 == s2 && s1 < 7 =
        let commonNext = stepSingle (afaMap afaCommonPart) s1 c
        in S.fromList [(s, s) | s <- S.toList commonNext, s <= 7]
    | otherwise =
        let nextStates1 = stepSingle (afaMap afa1) s1 c
            nextStates2 = stepSingle (afaMap afa2) s2 c
        in S.cartesianProduct nextStates1 nextStates2

accept1 :: S.Set Int
accept1 = S.fromList [26, 29]

accept2 :: S.Set Int  
accept2 = S.fromList [24, 25, 26, 29]

allAcceptPairs :: S.Set StatePair
allAcceptPairs = S.filter (\(s1, s2) -> s1 `S.member` accept1 && s2 `S.member` accept2)
                 (S.cartesianProduct accept1 accept2)

afaConfig :: AFAConfig
afaConfig = AFAConfig
    { afaStartState = (0, 0)
    , afaAcceptStates = allAcceptPairs
    , afaStepFunction = stepPair
    }

generateByAFA' :: Int -> IO String
generateByAFA' = generateByAFA afaConfig

isAcceptedByAFA :: String -> Bool
isAcceptedByAFA w = not (S.null (S.intersection finalStates (afaAcceptStates afaConfig)))
  where
    startSet = S.singleton (afaStartState afaConfig)
    
    finalStates = foldl step startSet w
      where
        step currentStates char = 
            S.unions [ afaStepFunction afaConfig statePair char 
                     | statePair <- S.toList currentStates ]

---------------------------------------------------------------------
-- main
---------------------------------------------------------------------
main :: IO ()
main = do
    let maxLength = 20

    word0 <- generateWord maxLength
    word1 <- generateByRegex
    word2 <- generateByDFA' maxLength
    word3 <- generateByNFA' maxLength
    word4 <- generateByAFA' maxLength

    let words = [word0, word1, word2, word3, word4]

    forM_ (zip [1..] words) $ \(i, word) -> do
        putStrLn ""
        putStrLn $ show word
        putStrLn $ "  regex: " ++ show (isAcceptedByRegex word) 
        putStrLn $ "  DFA:   " ++ show (isAcceptedByDFA word)
        putStrLn $ "  NFA:   " ++ show (isAcceptedByNFA word)
        putStrLn $ "  AFA:   " ++ show (isAcceptedByAFA word)
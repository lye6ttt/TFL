import System.Random
import System.IO
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Array
import Text.Regex.TDFA
import Data.Maybe (fromMaybe)

generateWord :: Int -> IO String
generateWord maxLength = do
    length <- randomRIO (1, maxLength)
    generateRandomWord length

generateRandomWord :: Int -> IO String
generateRandomWord n = replicateM n $ do
    let alphabet = "abc"
    i <- randomRIO (0, 2)
    pure (alphabet !! i)

generateByRegex :: Int -> IO String
generateByRegex maxLength = do
    let alphabet = "abc"
    length <- randomRIO (1, maxLength)

    findMatchingWord length alphabet
  where
    findMatchingWord len alph = do
        word <- replicateM len $ do
            i <- randomRIO (0, length alph - 1)
            pure (alph !! i)
        if isAcceptedByRegex word
            then return word
            else findMatchingWord len alph

generateByDFA :: Int -> IO String
generateByDFA maxLength = do
    let start = 0
        acc   = S.fromList [3,9,26,49,50,51]

    generateDFAPath start acc "" maxLength
  where
    generateDFAPath current accStates path maxLen
        | length path >= maxLen = if current `S.member` accStates 
                                  then return path 
                                  else generateByDFA maxLength  -- тупик
        | current `S.member` accStates = do
            stop <- randomRIO (0, 1 :: Int)
            if stop == 0 || length path >= maxLen
                then return path
                else continueGeneration
        | otherwise = continueGeneration
      where
        continueGeneration = do
            let possibleTransitions = [ (c, next) | c <- "abc", 
                                    let next = stepDFA current c, 
                                    next /= trap ]
            if null possibleTransitions
                then generateByDFA maxLength  -- тупик
                else do
                    idx <- randomRIO (0, length possibleTransitions - 1)
                    let (char, nextState) = possibleTransitions !! idx
                    generateDFAPath nextState accStates (path ++ [char]) maxLen

generateByNFA :: Int -> IO String
generateByNFA maxLength = do
    let start = S.singleton 0
        acc   = S.fromList [26,29]

    generateNFAPath start acc "" maxLength
  where
    generateNFAPath currentStates accStates path maxLen
        | length path >= maxLen = 
            if not (S.null (S.intersection currentStates accStates))
                then return path 
                else generateByNFA maxLength  -- тупик
        | not (S.null (S.intersection currentStates accStates)) = do
            stop <- randomRIO (0, 1 :: Int)
            if stop == 0 || length path >= maxLen
                then return path
                else continueGeneration
        | otherwise = continueGeneration
      where
        continueGeneration = do
            let allPossible = [ (c, nextState) | 
                                state <- S.toList currentStates,
                                (c, nextState) <- M.findWithDefault [] state nfaMap ]
            
            if null allPossible
                then generateByNFA maxLength  -- тупик
                else do
                    let byChar = M.fromListWith (++) [ (c, [ns]) | (c, ns) <- allPossible ]

                    let chars = M.keys byChar
                    charIdx <- randomRIO (0, length chars - 1)
                    let chosenChar = chars !! charIdx
                    let nextStates = S.fromList (byChar M.! chosenChar)
                    
                    generateNFAPath nextStates accStates (path ++ [chosenChar]) maxLen

generateByAFA :: Int -> IO String
generateByAFA maxLength = do
    let start = S.singleton 0
        acc1  = S.fromList [26,29]
        acc2  = S.fromList [24,25,26,29]

    generateAFAPath start acc1 acc2 "" maxLength
  where
    generateAFAPath currentStates acc1States acc2States path maxLen
        | length path >= maxLen = 
            if not (S.null (S.intersection currentStates acc1States)) &&
               not (S.null (S.intersection currentStates acc2States))
                then return path 
                else generateByAFA maxLength  -- тупик
        | not (S.null (S.intersection currentStates acc1States)) &&
          not (S.null (S.intersection currentStates acc2States)) = do
            stop <- randomRIO (0, 1 :: Int)
            if stop == 0 || length path >= maxLen
                then return path
                else continueGeneration
        | otherwise = continueGeneration
      where
        continueGeneration = do
            let allPossible1 = [ (c, nextState) | 
                                state <- S.toList currentStates,
                                (c, nextState) <- M.findWithDefault [] state afa1Map ]
                allPossible2 = [ (c, nextState) | 
                                state <- S.toList currentStates,
                                (c, nextState) <- M.findWithDefault [] state afa2Map ]
            
            let chars1 = S.fromList $ map fst allPossible1
                chars2 = S.fromList $ map fst allPossible2
                commonChars = S.toList $ S.intersection chars1 chars2
            
            if null commonChars
                then generateByAFA maxLength  -- тупик
                else do
                    charIdx <- randomRIO (0, length commonChars - 1)
                    let chosenChar = commonChars !! charIdx
                    
                    let nextStates1 = S.fromList [ ns | (c, ns) <- allPossible1, c == chosenChar ]
                    let nextStates2 = S.fromList [ ns | (c, ns) <- allPossible2, c == chosenChar ]
                    
                    let nextStates = S.union nextStates1 nextStates2
                    
                    generateAFAPath nextStates acc1States acc2States (path ++ [chosenChar]) maxLen

isAcceptedByRegex :: String -> Bool
isAcceptedByRegex w =
    let re = "(aa|bb|cc)*b(aaa|bbb)*((ab|bc|ccc)*aa)*abc(a|b|c)(b|())" :: String
    in  w =~ ('^' : re ++ "$")

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

isAcceptedByDFA :: String -> Bool
isAcceptedByDFA w =
    let start = 0
        acc   = S.fromList [3,9,26,49,50,51]
        final = foldl stepDFA start w
    in  final `S.member` acc

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

isAcceptedByNFA :: String -> Bool
isAcceptedByNFA w =
    let start = S.singleton 0
        acc   = S.fromList [26,29]
        final = foldl stepNFA start w
    in not (S.null (S.intersection final acc))
    
afa1 :: [(Int, Char, Int)]
afa1 =
  [ (0,'a',1),(0,'b',3),(0,'c',5),(0,'b',7)
  , (1,'a',0)
  , (3,'b',0)
  , (5,'c',0)
  , (7,'a',7),(7,'b',7),(7,'a',14),(7,'b',16),(7,'c',18)
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
  , (23,'b',24)
  , (24,'a',25),(24,'b',25),(24,'c',25)
  , (25,'a',26),(25,'b',26),(25,'c',26)
  , (26,'a',29),(26,'b',29),(26,'c',29)
  ]

afa1Map :: M.Map Int [(Char, Int)]
afa1Map = M.fromListWith (++) [ (s, [(c,t)]) | (s,c,t) <- afa1 ]

stepAFA1 :: S.Set Int -> Char -> S.Set Int
stepAFA1 states c =
    let step st = S.fromList $ map snd $ filter ((==c) . fst) $
                  M.findWithDefault [] st afa1Map
    in S.unions (map step $ S.toList states)

afa2Map :: M.Map Int [(Char, Int)]
afa2Map = M.fromListWith (++) [ (s, [(c,t)]) | (s,c,t) <- afa2 ]

stepAFA2 :: S.Set Int -> Char -> S.Set Int
stepAFA2 states c =
    let step st = S.fromList $ map snd $ filter ((==c) . fst) $
                  M.findWithDefault [] st afa2Map
    in S.unions (map step $ S.toList states)

isAcceptedByAFA :: String -> Bool
isAcceptedByAFA w =
    let start  = S.singleton 0
        acc1   = S.fromList [26,29]
        final1 = foldl stepAFA1 start w
        acc2   = S.fromList [24,25,26,29]
        final2 = foldl stepAFA2 start w
    in not (S.null (S.intersection final1 acc1)) &&
       not (S.null (S.intersection final2 acc2))


main :: IO ()
main = do
    let maxLength = 20

    word1 <- generateByRegex maxLength
    word2 <- generateByDFA maxLength
    word3 <- generateByNFA maxLength
    word4 <- generateByNFA maxLength

    let words = [word1, word2, word3, word4]

    forM_ (zip [1..] words) $ \(i, word) -> do
        let r   = isAcceptedByRegex word
            dfa = isAcceptedByDFA word
            nfa = isAcceptedByNFA word
            afa = isAcceptedByAFA word

        putStrLn $ show word
        putStrLn $ show r ++ show dfa ++ show nfa ++ show afa
        putStrLn ""
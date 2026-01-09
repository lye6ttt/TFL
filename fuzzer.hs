import System.Random (randomRIO)
import Data.List (reverse)
import System.CPUTime
import Text.Printf
import Control.DeepSeq

----------------------------
-- генерация случайных слов
----------------------------

alphabet :: [Char]
alphabet = ['a', 'b']

maxAttempts :: Int
maxAttempts = 15

genRandomChar :: IO Char
genRandomChar = do
  i <- randomRIO (0, length alphabet - 1)
  return (alphabet !! i)

genRandomWord :: Int -> IO String
genRandomWord n = sequence (replicate n genRandomChar)

genPalindrom :: Int -> IO String
genPalindrom len = do
  x <- genRandomWord (len `div` 2)
  if even len
    then return (x ++ reverse x)
    else do
      c <- genRandomChar
      return (x ++ [c] ++ reverse x)

uglifizeWord :: String -> IO String
uglifizeWord [] = return []
uglifizeWord s = do
  i <- randomRIO (0, length s - 1)
  let (prefix, c:suffix) = splitAt i s
      c' = if c == 'b' then 'a' else 'b'
  return (prefix ++ c' : suffix)


-- подходящих языку
genValid :: Int -> IO String
genValid n0 = do
  let n = if even n0 then n0 + 1 else n0
      lengthXY = (n - 3) `div` 2
  lenX <- randomRIO (0, lengthXY)
  let lenY = lengthXY - lenX

  let attempt 0 = do
        r <- randomRIO (0, 1 :: Int)
        return $
          if r == 0
            then replicate lenX 'a' ++ "c" ++ replicate (2 * lenY) 'a' ++ "ba" ++ replicate lenX 'a'
            else replicate lenX 'b' ++ "c" ++ replicate (2 * lenY) 'b' ++ "ba" ++ replicate lenX 'b'
      attempt k = do
        x <- genPalindrom lenX
        y <- genPalindrom lenY
        if x ++ "ab" ++ y == y ++ "ba" ++ x
          then return (x ++ "c" ++ y ++ y ++ "ba" ++ x)
          else attempt (k - 1)

  attempt maxAttempts

-- неподходящих языку
genInvalid :: Int -> IO String
genInvalid n = do
  randommaker <- randomRIO (0, 1 :: Int)
  case randommaker of
    0 -> do
      v <- genValid n
      uglifizeWord v
    1 -> do
      lenX <- randomRIO (0, (n - 3) `div` 2)
      x <- genPalindrom lenX
      y <- genRandomWord ((n - 3) `div` 2 - lenX + 1)
      return (take n (x ++ "c" ++ y ++ y ++ "ba" ++ x))


----------------------------
-- наивный парсер
----------------------------

naiveParse :: String -> Bool
naiveParse s = go (s ++ "$") 0 ""
  where
    go :: String -> Int -> String -> Bool
    go str idx x
      | str !! idx == 'c' = yPredict str (idx + 1) x 0
      | str !! idx == '$' = False
      | otherwise        = go str (idx + 1) (x ++ [str !! idx])


yPredict :: String -> Int -> String -> Int -> Bool
yPredict s index x yLen =
  let (y, ind, ok) = takeY s index yLen
  in if not ok
        then False
        else if acceptTail s ind x y
                then True
                else yPredict s index x (yLen + 1)
  where
    takeY :: String -> Int -> Int -> (String, Int, Bool)
    takeY str i 0 = ("", i, True)
    takeY str i n
      | str !! i == '$' = ("", i, False)
      | otherwise =
          let (ys, j, ok) = takeY str (i + 1) (n - 1)
          in (str !! i : ys, j, ok)


acceptTail :: String -> Int -> String -> String -> Bool
acceptTail s index x y = check index 0
  where
    lookahead = x ++ "ab" ++ y ++ "$"
    tailStr   = y ++ "ba" ++ x ++ "$"

    check :: Int -> Int -> Bool
    check i j
      | s !! i == '$' && lookahead !! j == '$' && tailStr !! j == '$' = True
      | s !! i == '$' || lookahead !! j == '$' || tailStr !! j == '$' = False
      | s !! i /= lookahead !! j || s !! i /= tailStr !! j           = False
      | otherwise = check (i + 1) (j + 1)


----------------------------
-- оптимизированный парсер
----------------------------

optimizedParse :: String -> Bool
optimizedParse s =
  let n = length s
  in if even n
     then False
     else
       let l0 = (n - 3) `div` 2
           s' = s ++ "$"
       in parseX s' 0 "" l0


parseX :: String -> Int -> String -> Int -> Bool
parseX s i x l
  | s !! i == '$' = False
  | s !! i == 'c' =
      if l < 0 || x /= reverse x
      then False
      else parseY s (i + 1) x "" l
  | otherwise =
      parseX s (i + 1) (x ++ [s !! i]) (l - 1)


parseY :: String -> Int -> String -> String -> Int -> Bool
parseY s i x y l
  | l > 0 =
      parseY s (i + 1) x (y ++ [s !! i]) (l - 1)
  | otherwise =
      if y /= reverse y
      then False
      else
        let lookahead = x ++ "ab" ++ y
            tailStr   = y ++ "ba" ++ x
        in if lookahead /= reverse lookahead
              || tailStr /= reverse tailStr
           then False
           else parseTail s i lookahead tailStr 0


parseTail :: String -> Int -> String -> String -> Int -> Bool
parseTail s i lookahead tailStr l
  | s !! i == '$' = True
  | s !! i /= lookahead !! l || s !! i /= tailStr !! l = False
  | otherwise =
      parseTail s (i + 1) lookahead tailStr (l + 1)


----------------------------
-- замер времени
----------------------------

timeIt :: NFData a => String -> a -> IO ()
timeIt label expr = do
    start <- getCPUTime
    expr `deepseq` return ()
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "%s: %.6f sec\n" label (diff :: Double)


benchmark :: Int -> Int -> IO ()
benchmark count l = do
    valid   <- sequence [genValid l   | _ <- [1..count]]
    invalid <- sequence [genInvalid l | _ <- [1..count]]

    putStrLn $ "\n=== VALID WORDS " ++ show l ++ " LENGTH ==="
    timeIt "naive parser"     (map naiveParse valid)
    timeIt "optimized parser" (map optimizedParse valid)

    putStrLn $ "\n=== INVALID WORDS " ++ show l ++ " LENGTH ==="
    timeIt "naive parser"     (map naiveParse invalid)
    timeIt "optimized parser" (map optimizedParse invalid)


----------------------------
-- фаззинг
----------------------------

fuzzing :: Int -> Int -> IO ()
fuzzing count l = do
  valid   <- sequence [genValid l   | _ <- [1..count]]
  invalid <- sequence [genInvalid l | _ <- [1..count]]
  let samples = zip (repeat "VALID") valid ++ zip (repeat "INVALID") invalid

  let go :: Int -> [(String, String)] -> IO ()
      go _ [] = putStrLn $ "OK: parsers are equivalent on " ++ show (2 * count)
                        ++ " samples (length=" ++ show l ++ ")"
      go k ((tag,w):ws) = do
        let r1 = naiveParse w
            r2 = optimizedParse w
        if r1 == r2
          then go (k + 1) ws
          else do
            putStrLn $ "OH NO ERROR on sample #" ++ show k ++ " (" ++ tag ++ ")"
            putStrLn $ "word = " ++ w
            putStrLn $ "naiveParse      = " ++ show r1
            putStrLn $ "optimizedParse  = " ++ show r2
            error "fuzzing failed: parsers are not equivalent"

  go 1 samples



main :: IO ()
main = do
    fuzzing 100 501

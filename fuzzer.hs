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



main :: IO ()
main = do
    putStrLn $ show (naiveParse "bababbababcbababbababbabababbabab")
    putStrLn $ show (optimizedParse "bababbababcbababbababbabababbabab")

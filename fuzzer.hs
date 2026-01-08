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

main :: IO ()
main = do
    putStrLn $ show (naiveParse "bababbababcbababbababbabababbabab")
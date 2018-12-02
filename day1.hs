solve :: [String] -> Int
solve [] = 0
solve (('+':n):xs) = read n + solve xs
solve (('-':n):xs) = solve xs - read n
solve _ = error "unexpected input"

toInt :: String -> Int
toInt ('+':n) = read n
toInt ('-':n) = negate $ read n
toInt n = read n

solvePart2' :: [Int]  -> [Int] -> (Maybe Int, [Int])
solvePart2' init = foldl doubleFreq (Nothing, init)
  where doubleFreq (Just found, _) _ = (Just found, [])
        doubleFreq (Nothing, []) _ = error "Unexpected input"
        doubleFreq (Nothing, x:seen) delta =
          let last = x + delta in
          if last `elem` seen then (Just last, [])
          else (Nothing, last:x:seen)

solvePart2 init input =
  let (found, state) = solvePart2' init input in
  case found of
    Just found -> found
    Nothing -> solvePart2 state input

input :: IO [Int]
input = do
  f <- readFile "day1.txt"
  return $ (map toInt . words) f
-- main = interact $ show . solvePart2 . map toInt . words

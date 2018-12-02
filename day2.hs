import Data.List

input :: IO [String]
input = do
  f <- readFile "day2.txt"
  return $ words f

solve1 :: [String] -> Int
solve1 ids =
    product
    $ foldl (\(s1:s2:_) (e1:e2:_) -> [inc s1 e1, inc s2 e2]) [0,0]
    $ map app
    $ map (flip (:) [])
    $ map (map length . group . sort) ids
  where app = (<*>) [any (== 2), any (== 3)]
        inc s e = s + if e then 1 else 0

solve2 :: [String] -> [String]
solve2 input = [ common
               | (s1, i) <- zip input [1..]
               , (s2, j) <- zip input [1..]
               , i < j
               , let common = foldl (++) "" $ zipWith getCommon s1 s2
               , length common == 25
               ]
  where getCommon s1 s2 = if s1 == s2 then s1:[] else ""

solve :: IO ()
solve = do
    input' <- input
    putStrLn $ "Part 1: " ++ (show . solve1) input'
    putStrLn $ "Part 2: " ++ (head . solve2) input'

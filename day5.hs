
import Data.Char(ord)
import Data.List

reduce :: String -> String -> String
reduce end start =
    case start' of
      "" -> reverse end'
      c:cs -> reduce (c:end') cs
  where start' = drop destroyed start
        end' = drop destroyed end
        destroyed = length $ takeWhile react $ zip start end
        react (c1, c2) = abs(ord c1 - ord c2) == 32

solve1 :: String -> Int
solve1 = length . reduce ""

solve2 :: String -> Int
solve2 input = minimum [(solve1 $ remove u input) | u <- ['a'..'z']]
  where remove u = filter (react u)
        react u c = abs(ord u - ord c) /= 32 && u /= c

input :: IO String
input = readFile "day5.txt" >>= return . head . words

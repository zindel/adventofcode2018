import Data.Char
import Data.List (minimumBy, partition)
import qualified Data.Set as S
import qualified Data.Map as M


type Dep = (Char, Char)

inputTest :: IO [Dep]
inputTest = readFile "day7_test.txt" >>= return . map toStep . lines
  where toStep = toStep' . filter isUpper
        toStep' (_:f:t:_) = (f,t)
        toStep' _ = error "Unexpected input"

input :: IO [Dep]
input = readFile "day7.txt" >>= return . map toStep . lines
  where toStep = toStep' . filter isUpper
        toStep' (_:f:t:_) = (f,t)
        toStep' _ = error "Unexpected input"

getSteps :: [Dep] -> [Char]
getSteps deps = S.toList $ S.fromList $ map fst deps ++ map snd deps

getDepMap :: [Dep] -> M.Map Char String
getDepMap = foldl (\m (v, k) -> M.insertWith (++) k (v:"") m) M.empty


removeDepOn :: Char -> M.Map Char String -> M.Map Char String
removeDepOn k = M.map $ filter (/= k)


order :: M.Map Char String -> String -> String
order _ "" = ""
order depMap steps = nextStep:(order (removeDepOn nextStep depMap) rest)
  where (nextStep, rest) = case span hasDeps steps of
          (_, "") -> error "impossible"
          (h, s:t) -> (s, h ++ t)
        hasDeps :: Char -> Bool
        hasDeps k = case M.lookup k depMap of
          Nothing -> False
          Just deps -> deps /= ""

solve1 :: [Dep] -> String
solve1 deps = order (getDepMap deps) (getSteps deps)

duration :: Int -> Char -> Int
duration p c = p + ord c - ord 'A' + 1

pickSteps workers df depMap todo inProgress = (todo', inProgress')
  where hasDeps k = case M.lookup k depMap of
          Nothing -> False
          Just deps -> deps /= ""
        freeSlots = workers - length inProgress
        collectSteps acc c =
          if length acc < freeSlots && not (hasDeps c)
          then c:acc
          else acc
        steps = foldl collectSteps "" todo
        todo' = filter (not . flip elem steps) todo
        inProgress' = inProgress ++ map (\c -> (c, df c)) steps

exeStep :: M.Map Char String -> [(Char, Int)] -> (Int, M.Map Char String, [(Char, Int)])
exeStep depMap inProgress = (min, depMap', inProgress')
  where min = snd $ minimumBy (\x1 x2 -> compare (snd x1) (snd x2)) inProgress
        (remove, keep) = partition ((== min) . snd) inProgress
        depMap' = foldl (flip removeDepOn) depMap $ map fst remove
        inProgress' = map (\(k, v) -> (k, v - min)) keep

exe workers df depMap todo inProgress =
  case pickSteps workers df depMap todo inProgress of
    ("", []) -> 0
    (todo', inProgress') ->
      let (min, depMap', inProgress'') = exeStep depMap inProgress' in
      min + exe workers df depMap' todo' inProgress''

solve2 workers p deps = exe workers (duration p) (getDepMap deps) (getSteps deps) []

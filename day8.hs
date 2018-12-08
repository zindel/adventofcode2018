import qualified Data.Map as M

data Node = Node { children :: [Node], metadata :: [Int] } deriving Show

getNode :: [Int] -> (Node, [Int])
getNode (n:m:rest) = (Node children $ take m rest', drop m rest')
  where (children, rest') = getNodes n rest
getNode _ = error "getNode: unexpected input"

getNodes :: Int -> [Int] -> ([Node], [Int])
getNodes 0 stream = ([], stream)
getNodes n stream = (node:nodes, rest')
  where (node, rest) = getNode stream
        (nodes, rest') = getNodes (n - 1) rest

sumNode :: Node -> Int
sumNode n = (sum $ metadata n) + (sum $ map sumNode $ children n)

valueNode :: Node -> Int
valueNode node =
    case children node of
      [] -> sum $ metadata node
      ch -> sumByIndex $ M.fromList $ zip [1..] $ map valueNode ch
  where sumByIndex m = sum $ map (maybe 0 id . flip M.lookup m) $ metadata node

solve1 :: [Int] -> Int
solve1 = sumNode . fst . getNode

solve2 :: [Int] -> Int
solve2 = valueNode . fst . getNode


input :: IO [Int]
input = readFile "day8.txt" >>= return . map read . words

inputTest :: IO [Int]
inputTest = readFile "day8_test.txt" >>= return . map read . words

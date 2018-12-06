import Data.List (sort, sortOn, group)
import Data.Maybe (fromJust)
import qualified Data.Set as S
--
--
-- y, x
type Point = (Int, Int)

data Bounds = Bounds { top :: Int, left :: Int, bottom :: Int, right :: Int } deriving (Show)
data Marker = Marker { id :: Int, point :: Point } deriving Show
data Cell = Cell { cellPoint :: Point
                 , closest :: Maybe Int
                 , total :: Int
                 , allDist :: [(Int, Int)]
                 , boundary :: Bool
                 } deriving Show

inputTest :: IO [Marker]
inputTest = readFile "day6_test.txt" >>= return . map toMarker . zip [1..] . lines
  where toMarker (id, line) = Marker id (read $ "(" ++ line ++ ")")

input :: IO [Marker]
input = readFile "day6.txt" >>= return . map toMarker . zip [1..] . lines
  where toMarker (id, line) = Marker id (read $ "(" ++ line ++ ")")

getBounds :: [Marker] -> Bounds
getBounds markers = Bounds {
      top = minimum ys
    , left = minimum xs
    , bottom = maximum ys
    , right = maximum xs
    }
  where xs = map (snd . point) markers
        ys = map (fst . point) markers

distance :: Point -> Point -> Int
distance (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)

getArea :: Bounds -> [Marker] -> [Cell]
getArea bounds markers =
    [ Cell { cellPoint = p
           , closest = getClosest ds
           , total = sum $ map snd ds
           , allDist = ds
           , boundary = y == top bounds || y == bottom bounds || x == left bounds || x == right bounds}
      | y <- [top bounds .. bottom bounds]
      , x <- [left bounds .. right bounds]
      , let p = (y,x)
      , let ds = sortOnDistance p
      ]
  where  getClosest :: [(Int, Int)] -> Maybe Int
         getClosest ds
          | length ds < 2 = error "Unexpected input"
          | d1:d2:_ <- ds = if snd d1 == snd d2 then Nothing else Just $ fst d1
          | otherwise = error "Should not get there"
         sortOnDistance cell =
           sortOn snd $ map (\m -> (Main.id m, distance cell $ point m)) markers

getBoundIds :: [Cell] -> S.Set (Maybe Int)
getBoundIds =
    S.fromList . map closest . filter (\c -> boundary c && closest c /= Nothing)

solve1 :: [Marker] -> Int
solve1 markers = head
                 $ reverse
                 $ sort
                 $ map length
                 $ group . sort
                 $ map closest
                 $ filter (\c -> not (closest c `S.member` boundIds)) area
  where bounds = getBounds markers
        area = filter (\c -> closest c /= Nothing) $ getArea bounds markers
        boundIds = getBoundIds area

-- manually verifed that boundary cells have total distance < 10000
solve2 :: [Marker] -> Int
solve2 markers = length $ filter (< 10000) $ map total area
  where bounds = getBounds markers
        area = getArea bounds markers

-- solve2 markers = [

--  ]
--   where bounds = getBounds markers


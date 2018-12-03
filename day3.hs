import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

data Claim = Claim { elfId :: Int
                   , left :: Int
                   , top :: Int
                   , width :: Int
                   , height :: Int
                   }
                   deriving (Show)

data Token = IntT Int
           | ClaimT Claim
type ParserState = ([Token], String)
type TokenStream = Either String ParserState

-- #166 @ 34,416: 22x15
instance Read Claim where
    readsPrec _ s =
      case parsed of
        Right([ClaimT claim], rest) -> [(claim, rest)]
        Left _ -> []
      where
        parsed = empty s
          >>= skip "#" >>= parseInt
          >>= skip " @ " >>= parseInt
          >>= skip "," >>= parseInt
          >>= skip ": " >>= parseInt
          >>= skip "x" >>= parseInt
          >>= parseClaim


empty :: String -> TokenStream
empty s = Right ([], s)

skip :: String -> ParserState -> TokenStream
skip s (tokens, rest)
  | length s > length rest = Left "ended too early"
  | all (uncurry (==)) $ zip s rest = Right (tokens, drop (length s) rest)
  | otherwise = Left "pattern not found"

parseInt :: ParserState -> TokenStream
parseInt (tokens, rest) =
  case takeWhile isDigit rest of
  "" -> Left "bad int literal"
  n -> Right (IntT(read n :: Int) : tokens, drop (length n) rest)

parseClaim :: ParserState -> TokenStream
parseClaim (tokens, rest) =
  case tokens of
  [IntT height, IntT width, IntT top, IntT left, IntT elfId] ->
    let claim = Claim { elfId = elfId
                      , left = left
                      , top = top
                      , width = width
                      , height = height
                      }
    in
    Right ([ClaimT claim], rest)
  _ -> Left "expected 5 ints"

squares :: Claim -> [(Int, Int)]
squares (Claim {left = left, top = top, width =width, height = height}) =
  [(x,y) | x <- [left + 1 .. left + width], y <- [top + 1 .. top + height]]

solve1 :: [Claim] -> Int
solve1 = M.size . M.filter (> 1) . foldl processClaim M.empty
  where processClaim m = foldl (\m sq -> M.insertWith (+) sq 1 m) m . squares

solve2 :: [Claim] -> S.Set Int
solve2 claims = S.difference (S.fromList $ map elfId claims) (overlaps claims)
  where processClaim m claim =
          foldl (\m sq -> M.insertWith S.union sq (S.singleton $ elfId claim) m) m
          $ squares claim
        overlaps = M.foldl S.union S.empty
                   . M.filter (\s -> S.size s > 1)
                   . foldl processClaim M.empty


input :: IO [Claim]
input = do
  f <- readFile "day3.txt"
  return $ map read $ lines f

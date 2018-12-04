import Data.Char
import Data.List (unfoldr, sortOn, groupBy)
import qualified Data.Map as M

data Event = Start | Sleep | WakeUp deriving (Show, Eq)
type TimeStamp = [Int]
data LogEntry = LogEntry { guardId :: Int
                         , ts :: TimeStamp
                         , event :: Event
                         } deriving (Show, Eq)


minute :: LogEntry -> Int
minute = head . reverse . ts

sleepWakeUp = count . filter ((/= Start) . event)
  where count list
          | [] <- list = []
          | (e1@LogEntry{event=Sleep, guardId=g1})
            :(e2@LogEntry{event=WakeUp, guardId=g2})
            :log <-list = if g1 == g2 then (g1, minute e2 - minute e1):(count log)
                                      else error "unexpected input"
          | otherwise = error "unexpected input"


-- solve1 :: [LogEntry] -> [(Int, Int)]
maxSleep = fst . head . reverse . sortOn snd . M.toList . foldl toMap M.empty . sleepWakeUp
  where toMap m (id, count) = M.insertWith (+) id count m

solve1 :: [LogEntry] -> Int
solve1 log = sleptOn * sleeper
  where sleptOn = fst $ head $ reverse $ sortOn snd $ M.toList $ count M.empty sleptLog
        sleeper = maxSleep log
        sleptLog = filter
          (\g -> guardId g == sleeper && (event g == Sleep || event g == WakeUp))
          log
        count m list
          | [] <- list = m
          | (e1@LogEntry{event=Sleep})
            :(e2@LogEntry{event=WakeUp})
            :log <-list = count (foldl toMap m [minute e1 .. (minute e2 - 1)]) log
          | otherwise = error "unexpected input"
        toMap m k = M.insertWith (+) k 1 m

solve2 :: [LogEntry] -> Int
solve2 log = guardId * m
  where ((guardId, m), _) = head $ reverse $ sortOn snd $ M.toList $ count M.empty sleptLog
        sleptLog = filter (\g -> event g == Sleep || event g == WakeUp) log
        count m list
          | [] <- list = m
          | (e1@LogEntry{event=Sleep, guardId=g1})
            :(e2@LogEntry{event=WakeUp, guardId=g2})
            :log <-list = if g1 == g2
                          then count (foldl (toMap g1) m [minute e1 .. (minute e2 - 1)]) log
                          else error "unexpected input"
          | otherwise = error "unexpected input"
        toMap id m k = M.insertWith (+) (id, k) 1 m

-- [1518-09-24 00:03] Guard #2137 begins shift
-- [1518-10-11 00:35] wakes up
-- [1518-07-10 00:59] wakes up
input :: IO [LogEntry]
input = do
    f <- readFile "day4.txt"
    return $ reverse $ snd $ foldl logEntry (Nothing, []) $ sortOn fst $ map parseTs $ lines f
  where parseTs :: String -> (TimeStamp, String)
        parseTs s =
            let (ts', rest) = splitAt 17 s in
            (unfoldr nextInt $ drop 1 ts', drop 2 rest)
        nextInt :: String -> Maybe(Int, String)
        nextInt s =
          case span isDigit s of
            ("", _) -> Nothing
            (n, rest) -> Just (read n, drop 1 rest)
        logEntry :: (Maybe Int, [LogEntry]) -> (TimeStamp, String) -> (Maybe Int, [LogEntry])
        logEntry (guardId, log) (ts, s) =
          let (id', event) = case (guardId, take 5 s) of
                  (_, "Guard") -> (read $ takeWhile isDigit $ drop 7 s, Start)
                  (Just id, "falls") -> (id, Sleep)
                  (Just id, "wakes") -> (id, WakeUp)
          in
          (Just id', (LogEntry {guardId=id', ts=ts, event=event}):log)

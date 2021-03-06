module Cleaner.CleaningVotes where

import Data.List
import Data.List (sortOn)
import Data.List.Split (splitOn)

cleanRoundOne :: String -> [[String]]
cleanRoundOne csvFile = map (splitOn ",") $ splitOn "\n" csvFile

removeVoteNumberAndName :: [[String]] -> [[String]]
removeVoteNumberAndName xs = [drop 2 x | x <- xs]

zipVotes :: [[String]] -> [[(Integer, String)]]
zipVotes xs = [zip [1 .. 5] x | x <- xs]

-- [1,2,3,4,5]
-- ["D. Abbott","E. Balls","A. Burbhm","D. Milliband","E. Milliband"]

-- [(4,"1"),(5,"2"),(3,"3"),(2,"4"),(1,"5")]
-- ["D. Milliband","E. Milliband","A. Burbhm","E. Balls","D. Abbott"]

-- [[(Candidate, candidatePreference)]]
sortVotes :: [[(Integer, String)]] -> [[(Integer, String)]]
sortVotes = map $ sortOn snd

getCandidates :: String -> [String]
getCandidates firstpass = drop 2 $ head (cleanRoundOne firstpass)

countVotes :: [[String]] -> Int
countVotes xs = length xs

-- -- get names on numbers in votes
-- [(4, "D. Milliband"), (3, "A. Burbhm"), (5, "E. Milliband"), (1, "D. Abbott"), (2, "E. Balls")]
zipCandidates :: [String] -> [(Integer, String)]
zipCandidates xs = zip [1 ..] xs

goMatch :: Integer -> [(Integer, String)] -> String
goMatch i [] = undefined
goMatch i ((i', n) : xs) = if i == i' then n else goMatch i xs

mapVotesAndCandidates :: [(Integer, String)] -> [[Integer]] -> [[String]]
mapVotesAndCandidates _ [] = []
mapVotesAndCandidates ns (is : iss) =
  map locate is : mapVotesAndCandidates ns iss
  where
    locate i = goMatch i ns

-- mapVotesAndCandidates [(1, "D. Abbott"), (2, "E. Balls")] [[1,2], [2]]
-- -> [["D. Abbott", "E. Balls"], ["E. Balls"]]

-- `ns` names it's the list of candidates, paired with numbers [(Integer, String)]
-- `ns` is the association list used by goMatch
-- something like this [(4, "D. Milliband"), (3, "A. Burbhm"), (5, "E. Milliband"), (1, "D. Abbott"), (2, "E. Balls")]

discardFromListOfLists :: (a -> Bool) -> [[a]] -> [[a]]
discardFromListOfLists p xs = map (discardFromList p) xs

discardFromList :: (a -> Bool) -> [a] -> [a]
discardFromList p xs = [x | x <- xs, not $ p x]

module Cleaner.CleaningVotes where

import Data.List
import Data.List.Split (splitOn)

cleanRoundOne :: String -> [[String]]
cleanRoundOne dirtyVotes = map (splitOn ",") $ splitOn "\n" dirtyVotes

removeVoteNumberAndName :: [[String]] -> [[String]]
removeVoteNumberAndName xs = [drop 2 x | x <- xs]

discardFromListOfLists :: (a -> Bool) -> [[a]] -> [[a]]
discardFromListOfLists p xs = map (discardFromList p) xs

discardFromList :: (a -> Bool) -> [a] -> [a]
discardFromList p xs = [x | x <- xs, not $ p x]

getCandidates :: String -> [String]
getCandidates firstpass = drop 2 $ head (cleanRoundOne firstpass)

countVotes :: [[String]] -> Int
countVotes xs = length xs

-- ["4","3","5","1","2"]

-- -- get names on numbers in votes
-- [("4", "D. Milliband"), ("3", "A. Burbhm"), ("5", "E. Milliband"), ("1", "D. Abbott"), ("2", "E. Balls")]
tupleCandidates :: [String] -> [(Integer, String)]
tupleCandidates xs = zip [1 ..] xs

stringListToInt :: [[String]] -> [[Integer]]
stringListToInt = map (map read)

goMatch :: Integer -> [(Integer, String)] -> String
goMatch i [] = undefined
goMatch i ((i', n) : xs) = if i == i' then n else goMatch i xs

mapVotesAndCandidates :: [(Integer, String)] -> [[Integer]] -> [[String]]
mapVotesAndCandidates _ [] = []
mapVotesAndCandidates ns (is : iss) =
  map locate is : mapVotesAndCandidates ns iss
  where
    locate i = goMatch i ns

module Cleaner.CleaningVotes where

import Data.List.Split (splitOn)

cleanRoundOne :: String -> [[String]]
cleanRoundOne dirtyVotes = map (splitOn ",") $ splitOn "\n" dirtyVotes

removeVoteNumberAndName :: [[String]] -> [[String]]
removeVoteNumberAndName xs = [drop 2 x | x <- xs]

discardFromListOfLists :: (a -> Bool) -> [[a]] -> [[a]]
discardFromListOfLists p xs = map (\list -> discardFromList p list) xs

discardFromList :: (a -> Bool) -> [a] -> [a]
discardFromList p xs = [x | x <- xs, not $ p x]

getCandidates :: String -> [String]
getCandidates firstpass = drop 2 $ head (cleanRoundOne firstpass)
module Cleaner.CleaningVotes where

import Data.List.Split (splitOn)

-- not yet implemented
cleanRoundOne :: String -> [[String]]
cleanRoundOne dirtyVotes = map (splitOn ",") $ splitOn "\n" dirtyVotes

getCandidates :: String -> [String]
getCandidates firstpass = drop 2 $ head (cleanRoundOne firstpass)
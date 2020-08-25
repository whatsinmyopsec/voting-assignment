module Cleaner.CleaningVotes where

import Data.List.Split (splitOn)

-- not yet implemented
cleanRoundOne :: String -> [[String]]
cleanRoundOne dirtyVotes = map (splitOn ",") $ splitOn "\r\n" dirtyVotes
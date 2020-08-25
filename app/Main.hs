module Main where

import Cleaner.CleaningVotes

main :: IO ()
main = do
  putStrLn "Please enter the file you would like to query for results:"
  file <- getLine

  csvData <- readFile file

  -- cleaning section --
  let firstPass = cleanRoundOne csvData
  print $ firstPass

  let candidates = getCandidates csvData
  putStrLn "The candidates are:\n"
  print $ candidates

-- stack ghci to test
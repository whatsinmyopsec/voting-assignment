module Main where

import Cleaner.CleaningVotes

main :: IO ()
main = do
  putStrLn "Please enter the file you would like to query for results:"
  file <- getLine

  csvData <- readFile file

  let candidates = getCandidates csvData
  putStrLn "The candidates are:\n"
  print $ candidates
  -- cleaning section --
  let firstPass = cleanRoundOne csvData
  let stars = discardFromListOfLists (== "*") firstPass
  let rmBlanks = discardFromListOfLists (== "") stars
  let rmNullLists = discardFromList (== []) rmBlanks
  let numbers = discardFromList (== candidates) rmNullLists
  let votes = removeVoteNumberAndName numbers
  print $ votes

-- stack ghci to test
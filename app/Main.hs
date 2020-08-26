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
  putStrLn "\n"
  -- cleaning section --
  let firstPass = cleanRoundOne csvData
  let rmStars = discardFromListOfLists (== "*") firstPass
  let rmBlanks = discardFromListOfLists (== "") rmStars
  let rmNullLists = discardFromList (== []) rmBlanks
  let numbers = discardFromList (== candidates) rmNullLists
  let votes = removeVoteNumberAndName numbers
  print $ votes
  let totalVotes = countVotes votes
  putStrLn "Total votes after cleaning:\n"
  print $ totalVotes

-- stack ghci to test
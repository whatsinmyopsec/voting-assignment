module Main where

import Cleaner.CleaningVotes
import Count.ATV

main :: IO ()
main = do
  putStrLn "Please enter the file you would like to query for results:"
  file <- getLine

  csvData <- readFile file

  let candidates = getCandidates csvData
  putStrLn "The candidates are: "
  print $ candidates
  putStrLn "\n"

  -- cleaning section --
  let firstPass = cleanRoundOne csvData
  let rmStars = discardFromListOfLists (== "*") firstPass
  let rmBlanks = discardFromListOfLists (== "") rmStars
  let rmNullLists = discardFromList (== []) rmBlanks
  let numbers = discardFromList (== candidates) rmNullLists
  let votes = removeVoteNumberAndName numbers
  let intList = stringListToInt votes
  let tuples = tupleCandidates candidates
  let endCleaningCandidates = mapVotesAndCandidates tuples intList
  print $ endCleaningCandidates

  -- ATV winner --
  putStr "ATV winner: "
  let atvWinner = winner' endCleaningCandidates
  -- ATV not correct yet
  print $ atvWinner
  let totalVotes = countVotes votes
  putStr "Total votes after cleaning: "
  print $ totalVotes

-- stack ghci to test
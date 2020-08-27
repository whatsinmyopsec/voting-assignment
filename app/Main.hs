module Main where

import Cleaner.CleaningVotes
import Count.AV

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
  let rmBlanks = discardFromListOfLists (== "") firstPass
  let rmNullLists = discardFromList (== []) rmBlanks
  let numbers = discardFromList (== candidates) rmNullLists
  let votes = removeVoteNumberAndName numbers
  let zipper = zipVotes votes
  let prefMan = sortVotes zipper
  -- This needs to be here to keep the preferece of the votes
  -- else its all messed up
  let rmStars = (map $ discardFromList ((== "*") . snd)) prefMan
  print $ rmStars
  let fixVotes = discardFromListOfLists (== "*") votes
  let intList = stringListToInt fixVotes

  let tuples = tupleCandidates candidates

  let endCleaningCandidates = mapVotesAndCandidates tuples intList
  print $ endCleaningCandidates

  -- AV winner --
  putStr "Alternative Vote winner: "
  let avWinner = winner' endCleaningCandidates
  -- AV not correct yet
  print $ avWinner
  let totalVotes = countVotes votes
  putStr "Total votes after cleaning: "
  print $ totalVotes

-- stack ghci to test
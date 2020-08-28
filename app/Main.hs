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
  mapM_ print $ candidates

  putStrLn "Cleaning in process\n"

  -- Cleaning section --
  let firstPass = cleanRoundOne csvData
  let rmBlanks = discardFromListOfLists (== "") firstPass
  let rmNullLists = discardFromList (== []) rmBlanks
  let numbers = discardFromList (== candidates) rmNullLists
  let votes = removeVoteNumberAndName numbers
  let zipper = zipVotes votes
  let prefMan = sortVotes zipper
  let rmStars = (map $ discardFromList ((== "*") . snd)) prefMan
  let tuples = zipCandidates candidates
  let lists = map (map fst) rmStars
  let endCleaningCandidates = mapVotesAndCandidates tuples lists

  -- After cleaning total votes amount --
  let totalVotes = countVotes votes
  putStr "Total votes after cleaning: "
  print $ totalVotes

  -- AV winner --
  putStr "Alternative Vote winner: "
  let avWinner = winner' endCleaningCandidates
  print $ avWinner

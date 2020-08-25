module Main where

import Cleaner.CleaningVotes

main :: IO ()
main = do
  let file = "Please enter the file you would like to query for results:"
  print file

  csvData <- readFile file

  -- cleaning section --
  let firstPass = cleanRoundOne csvData
  print firstPass
module Main (main) where


import Dictionary

main :: IO ()
main = do
  let emptyDict = createEmptyDictionary :: Dictionary Int String
  print emptyDict
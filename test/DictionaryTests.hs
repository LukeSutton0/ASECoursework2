module DictionaryTests (dictionaryTests) where

import Test.HUnit
import Test.QuickCheck
import Dictionary

-- | Test cases for `Dictionary Tests` 
dictionaryTests :: Test
dictionaryTests = TestList [
  testCreateEmptyDictionary
  ]

testCreateEmptyDictionary :: Test
testCreateEmptyDictionary = TestCase $ do
  let dict = createEmptyDictionary :: Dictionary Int String
  assertEqual "createEmptyDictionary, should return an empty dict" createEmptyDictionary dict

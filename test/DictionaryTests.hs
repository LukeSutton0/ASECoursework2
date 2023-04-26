module DictionaryTests (createDictionaryTests) where

import Test.HUnit
import Dictionary

-- | Test cases for `Dictionary Tests` 
createDictionaryTests :: Test
createDictionaryTests = TestList [
  testCreateEmptyDictionary,
  testInsertIntoDictionary
  ]

testCreateEmptyDictionary :: Test
testCreateEmptyDictionary = TestCase $ do
  let dict = createEmptyDictionary :: [(Int, String)]
  assertEqual "createEmptyDictionary, should return an empty dict" [] dict


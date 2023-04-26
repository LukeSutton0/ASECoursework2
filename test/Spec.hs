import Test.HUnit

import Lib --import so that we can test functions in Lib.hs
import DictionaryTests (dictionaryTests)
import BSTTests (binarySearchTreeTests)

main :: IO ()
main = do
    runTestTT allTestsInSpec

    _ <- runTestTT dictionaryTests

    _ <- runTestTT binarySearchTreeTests
    return()


allTestsInSpec :: Test
allTestsInSpec = TestList [firstTest]

firstTest :: Test
firstTest = TestCase (assertEqual "An example test" 9 (sqr 3))
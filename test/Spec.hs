import Test.HUnit
import Test.QuickCheck
import Lib --import so that we can test functions in Lib.hs
import DictionaryTests (dictionaryTests)
import BSTTests (binarySearchTreeTests,binarySearchTreeQuickTests)

main :: IO ()
main = do
    -- runTestTT allTestsInSpec --use this for tests in this file
    _ <- runTestTT binarySearchTreeTests
    quickCheck binarySearchTreeQuickTests
    -- _ <- runTestTT dictionaryTests    
    return()


allTestsInSpec :: Test
allTestsInSpec = TestList [firstTest]

firstTest :: Test
firstTest = TestCase (assertEqual "An example test" 9 (sqr 3))
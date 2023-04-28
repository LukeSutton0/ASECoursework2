import Test.HUnit
import Test.QuickCheck
import Lib --import so that we can test functions in Lib.hs
import DictionaryTests (dictionaryMain)
import BSTTests (bSTreeMain)

main :: IO ()
main = do
    -- runTestTT allTestsInSpec --use this for tests in this file
    bSTreeMain
    dictionaryMain
    return()


allTestsInSpec :: Test
allTestsInSpec = TestList [firstTest] --add more tests here if need

firstTest :: Test
firstTest = TestCase (assertEqual "An example test" 9 (sqr 3))
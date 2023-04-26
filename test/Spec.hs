import Test.HUnit

import Lib --import so that we can test functions in Lib.hs
import DictionaryTests (createDictionaryTests)

main :: IO ()
main = do
    runTestTT allTestsInSpec
    _ <- runTestTT createDictionaryTests
    return()


allTestsInSpec :: Test
allTestsInSpec = TestList [firstTest]

firstTest :: Test
firstTest = TestCase (assertEqual "Some words explaining test," 9 (sqr 3))
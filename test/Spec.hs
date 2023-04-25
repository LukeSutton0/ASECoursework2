import Test.HUnit

import Lib --import so that we can test functions in Lib.hs
import BinaryTreeTests (createBinaryTreeTests)

main :: IO ()
main = do
    runTestTT allTestsInSpec
    _ <- runTestTT createBinaryTreeTests
    return()


allTestsInSpec :: Test
allTestsInSpec = TestList [firstTest]

firstTest :: Test
firstTest = TestCase (assertEqual "Some words explaining test," 9 (sqr 3))
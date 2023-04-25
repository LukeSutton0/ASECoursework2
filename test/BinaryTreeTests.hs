module BinaryTreeTests (createBinaryTreeTests) where
import Test.HUnit
import BinaryTree

-- | Test cases for `createBinaryTree` function
createBinaryTreeTests :: Test
createBinaryTreeTests = TestList [
    testEmptyList,
    testSingleElement,
    testEvenElements,
    testOddElements
  ]

testEmptyList :: Test
testEmptyList = TestCase $ do
  assertEqual "empty list should return empty tree" EmptyTree (createBinaryTree ([] :: [Int]))

testSingleElement :: Test
testSingleElement = TestCase $ do
  assertEqual "tree with one element should return a leaf" (Node 1 EmptyTree EmptyTree) (createBinaryTree [1])

testEvenElements :: Test
testEvenElements = TestCase $ do
  assertEqual "tree with even number of elements should have correct structure" (Node 3 (Node 1 EmptyTree EmptyTree) (Node 5 EmptyTree EmptyTree)) (createBinaryTree [1,3,5])

testOddElements :: Test
testOddElements = TestCase $ do
  assertEqual "tree with odd number of elements should have correct structure" (Node 3 (Node 1 EmptyTree EmptyTree) (Node 5 (Node 4 EmptyTree EmptyTree) EmptyTree)) (createBinaryTree [1,3,4,5])

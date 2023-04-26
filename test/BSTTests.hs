module BSTTests (binarySearchTreeTests) where

import Test.HUnit
import Test.QuickCheck
import BSTree

-- | Test cases for `Dictionary Tests` 
binarySearchTreeTests :: Test
binarySearchTreeTests = TestList [
  testCreateEmptyBSTree,
  testInsertIntoBSTree
  ]

testCreateEmptyBSTree :: Test
testCreateEmptyBSTree = TestCase $ do
  let aTree = createEmptyBSTree :: BSTree Int String
  assertEqual "createEmptyBSTree, should return an empty dict" createEmptyBSTree aTree

testInsertIntoBSTree :: Test
testInsertIntoBSTree = TestCase $ do
    let aTree = createEmptyBSTree :: BSTree Int String
    let aTreeWithOneNode = insertIntoBSTree 1 "one" aTree
    assertEqual "insertIntoBSTree, should return a tree with one node" (Node 1 "one" Empty Empty) aTreeWithOneNode


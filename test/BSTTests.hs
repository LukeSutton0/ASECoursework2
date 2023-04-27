module BSTTests (bSTreeMain) where

import Test.HUnit
import Test.QuickCheck
import BSTree

-- Test cases for `binary Search Tree Tests` 
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
    let aTreeWithANode = insertIntoBSTree 1 "My Value for the inserted node" createEmptyBSTree :: BSTree Int String
    assertEqual "insertIntoBSTree, should return a tree with one node" 
      (Node 1 "My Value for the inserted node" Empty Empty) aTreeWithANode

-- QuickCheck tests for `binary Search Tree Tests`
-- Generate binary search trees with at most `n` nodes

-- Generate a random BSTree with unique keys



instance (Arbitrary key, Arbitrary item) => Arbitrary (BSTree key item) where
  arbitrary = sized genTree

genTree n 
  | n > 0 = do
    key <- arbitrary
    item <- arbitrary
    left <- genTree (n `div` 2)
    right <- genTree (n `div` 2)
    return (Node key item left right)
  | otherwise = return Empty --Empty or Node


prop_lookupBSTree :: BSTree Int String -> Bool
prop_lookupBSTree tree =
  let keyValues = toList tree
  in case keyValues of
       [] -> lookupBSTree 0 tree == Nothing
       _  -> let (k, v) = head keyValues
                 lookupResult = lookupBSTree k tree
             in case lookupResult of
                  Nothing -> False
                  Just value -> value == v
  where
    toList Empty = []
    toList (Node k v l r) = (k, v) : toList l ++ toList r





bSTreeMain :: IO ()
bSTreeMain = do
  _ <- runTestTT binarySearchTreeTests --run all HUnit tests
  -- quickCheck testInsertIntoBSTreeExists
  quickCheck prop_lookupBSTree
  --quickCheck testLookupBSTree
  return()
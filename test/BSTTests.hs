module BSTTests (bSTreeMain) where

import Test.HUnit
import Test.QuickCheck
import BSTree
import Debug.Trace
import Data.List
import Control.Monad
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
instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (BSTree k v) where
  arbitrary = sized genTree

genTree :: (Arbitrary k, Arbitrary v, Ord k) => Int -> Gen (BSTree k v)
genTree n  = do
    key <- genUniqueKeys n
    foldM (\tree k -> do
      v <- arbitrary
      return $ insertIntoBSTree k v tree) Empty key

genUniqueKeys :: (Arbitrary k, Eq k) => Int -> Gen [k]
genUniqueKeys n = fmap(take n . nub)(infiniteListOf arbitrary)



prop_createEmptyBSTree :: forall k v. (Ord k, Eq v) => BSTree k v -> k -> v -> Bool
prop_createEmptyBSTree _ _ _ = isEmptyBSTree (createEmptyBSTree :: BSTree k v)

isEmptyBSTree :: BSTree k v -> Bool
isEmptyBSTree Empty = True
isEmptyBSTree _ = False

prop_insertIntoBSTree :: (Ord k, Eq v) => k -> v -> BSTree k v -> Bool
prop_insertIntoBSTree key value tree =
  let updatedTree = insertIntoBSTree key value tree
  in lookupBSTree key updatedTree == Just value

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

prop_listBSTreeVals :: BSTree Int String -> Bool
prop_listBSTreeVals tree =
  let keyValues = listBSTreeVals tree
  in all (\(k, v) -> lookupBSTree k tree == Just v) keyValues
     where
       listBSTreeVals t = 
         case t of
           Empty -> []
           Node k v l r -> listBSTreeVals l ++ [(k, v)] ++ listBSTreeVals r

--listBSTreeVals t = trace ("listBSTreeVals: " ++ show t) $

bSTreeMain :: IO ()
bSTreeMain = do
  _ <- runTestTT binarySearchTreeTests --run all HUnit tests
  quickCheck (prop_insertIntoBSTree :: Int -> String -> BSTree Int String -> Bool)
  quickCheck prop_lookupBSTree
  quickCheck prop_listBSTreeVals
  quickCheck (prop_createEmptyBSTree :: BSTree Int String -> Int -> String -> Bool)
  --quickCheck testLookup

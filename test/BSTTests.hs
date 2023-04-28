{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module BSTTests (bSTreeMain) where

import Test.HUnit
import Test.QuickCheck
import BSTree
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
prop_insertIntoBSTree k v tree =
  let updatedTree = insertIntoBSTree k v tree
  in lookupBSTree k updatedTree == Just v

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

prop_removeFromBSTree :: (Ord k, Eq v) => k -> BSTree k v -> Bool
prop_removeFromBSTree k t =
  let t' = removeFromBSTree k t
  in not (memberBSTree k t') && all (\(k', v') -> lookupBSTree k' t' == Just v') (listBSTreeVals t')

memberBSTree :: Ord k => k -> BSTree k v -> Bool
memberBSTree _ Empty = False
memberBSTree k (Node k' _ l r)
  | k == k' = True
  | k < k'  = memberBSTree k l
  | otherwise = memberBSTree k r

prop_removeEntriesIf :: Ord k => (k -> v -> Bool) -> BSTree k v -> Bool
prop_removeEntriesIf p tree = all (\(k,v) -> not (p k v)) (listBSTreeVals tree')
    where tree' = removeEntriesIf p tree
    
instance Show (Int -> String -> Bool) where
  show _ = "<function>"

bSTreeMain :: IO ()
bSTreeMain = do
  _ <- runTestTT binarySearchTreeTests --run all HUnit tests
  quickCheck (prop_insertIntoBSTree :: Int -> String -> BSTree Int String -> Bool)
  quickCheck prop_lookupBSTree
  quickCheck prop_listBSTreeVals
  quickCheck (prop_createEmptyBSTree :: BSTree Int String -> Int -> String -> Bool)
  quickCheck (prop_removeFromBSTree :: Int -> BSTree Int String -> Bool)
  quickCheck (prop_removeFromBSTree :: Int -> BSTree Int String -> Bool)


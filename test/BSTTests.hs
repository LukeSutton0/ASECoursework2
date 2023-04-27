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
genBSTree :: Int -> Gen (BSTree Int String)
genBSTree n = genBSTree' n []
  where
    genBSTree' :: Int -> [Int] -> Gen (BSTree Int String)
    genBSTree' 0 _ = return Empty
    genBSTree' n usedKeys = frequency [
        (1, return Empty),
        (6, do 
            k <- choose (1, n)
            k <- uniqueKey k usedKeys
            v <- resize 5 $ listOf $ elements ['a'..'z']
            let newUsedKeys = k : usedKeys
            l <- genBSTree' (n `div` 2) newUsedKeys
            r <- genBSTree' (n `div` 2) newUsedKeys
            return $ Node k v l r)]

    uniqueKey :: Int -> [Int] -> Gen Int --make sure key is unique
    uniqueKey k usedKeys
      | k `elem` usedKeys = do
          newK <- choose (1, length usedKeys)
          uniqueKey newK usedKeys
      | otherwise = return k

testInsertIntoBSTreeExists :: Property
testInsertIntoBSTreeExists =
  forAll (genBSTree 100) $ \t -> -- random tree with at most 100 nodes
    forAll arbitrary $ \k -> -- random key
      forAll arbitrary $ \v -> -- random value
        let newT = insertIntoBSTree k v t -- insert key-value pair into tree
        in existsInBSTree k newT -- check if key exists in new tree

existsInBSTree :: Ord k => k -> BSTree k v -> Bool
existsInBSTree _ Empty = False
existsInBSTree k (Node k' _ l r)
  | k == k' = True
  | k < k' = existsInBSTree k l
  | otherwise = existsInBSTree k r

testLookupBSTree :: Property
testLookupBSTree =
  forAll (genBSTree 10) $ \t -> -- random tree with at most 100 nodes
    case t of
      Empty -> property True -- skip if tree is empty
      _ ->
        forAll arbitrary $ \k -> -- generates random key
          let expected = lookup k (getKeyValuePairs t) --tries to find the key in the tree if not found returns nothing
              actual = lookupBSTree k t --
          in counterexample (show (k, expected, actual)) $
             (existsInBSTree k t && actual == expected) || (not (existsInBSTree k t) && actual == Nothing)

getKeyValuePairs :: BSTree k v -> [(k, v)]
getKeyValuePairs Empty = []
getKeyValuePairs (Node k v l r) = getKeyValuePairs l ++ [(k, v)] ++ getKeyValuePairs r



bSTreeMain :: IO ()
bSTreeMain = do
  -- _ <- runTestTT binarySearchTreeTests --run all HUnit tests
  quickCheck testInsertIntoBSTreeExists
  --quickCheck testLookupBSTree
  quickCheck testLookupBSTree
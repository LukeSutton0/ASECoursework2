{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
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

genBSTree :: (Arbitrary k, Arbitrary v, Ord k) => Gen (BSTree k v)
genBSTree = sized $ \n ->
  if n == 0
    then return Empty
    else do
      (kl, vl, kr, vr) <- splitList n <$> arbitrary
      k <- arbitrary
      v <- arbitrary
      l <- genBSTree' kl
      r <- genBSTree' kr
      return $ Node k (v :: v) l r

genBSTree' :: (Arbitrary k, Arbitrary v, Ord k) => [Int] -> Gen (BSTree k v)
genBSTree' [] = return Empty
genBSTree' (x:xs) = do
  k <- arbitrary
  v <- arbitrary
  l <- genBSTree' [i | i <- xs, i < x]
  r <- genBSTree' [i - x - 1 | i <- xs, i > x]
  return $ Node k (v :: v) l r

testInsertIntoBSTreeExists :: Property
testInsertIntoBSTreeExists =
  forAll genBSTree $ \t -> 
    forAll arbitrary $ \k -> 
      forAll arbitrary $ \v -> 
        let newT = insertIntoBSTree k v t 
        in existsInBSTree k newT 

existsInBSTree :: Ord k => k -> BSTree k v -> Bool
existsInBSTree _ Empty = False
existsInBSTree k (Node k' _ l r)
  | k == k' = True
  | k < k' = existsInBSTree k l
  | otherwise = existsInBSTree k r

testLookupBSTree :: Property
testLookupBSTree =
  forAll genBSTree $ \t -> 
    case t of
      Empty -> property True 
      _ ->
        forAll arbitrary $ \k -> 
          let expected = lookup k (getKeyValuePairs t); actual = lookupBSTree k t

          in counterexample (show (k, expected, actual)) $
             (existsInBSTree k t && actual == expected) || (not (existsInBSTree k t) && actual == Nothing) 

getKeyValuePairs :: BSTree k v -> [(k, v)]
getKeyValuePairs Empty = []
getKeyValuePairs (Node k v l r) = getKeyValuePairs l ++ [(k, v)] ++ getKeyValuePairs r

--returns list of key value pairs





bSTreeMain :: IO ()
bSTreeMain = do
  -- _ <- runTestTT binarySearchTreeTests --run all HUnit tests
  quickCheck testInsertIntoBSTreeExists
  quickCheck testLookupBSTree
  --verboseCheck testLookupBSTree



  -- Generate binary search trees with at most `n` nodes
-- genBSTree :: (Arbitrary k, Ord k) => Int -> Gen (BSTree k String)
-- genBSTree n = genBSTree' n []
--   where
--     genBSTree' :: (Arbitrary k, Ord k) => Int -> [k] -> Gen (BSTree k String)
--     genBSTree' 0 _ = return Empty
--     genBSTree' n usedKeys = frequency [
--         (1, return Empty),
--         (6, do 
--             k <- uniqueKey usedKeys
--             v <- resize 5 $ listOf $ elements ['a'..'z']
--             let newUsedKeys = k : usedKeys
--             l <- genBSTree' (n `div` 2) newUsedKeys
--             r <- genBSTree' (n `div` 2) newUsedKeys
--             return $ Node k v l r)]

--     uniqueKey :: (Arbitrary k, Ord k) => [k] -> Gen k
--     uniqueKey usedKeys = do
--         k <- arbitrary
--         if k `elem` usedKeys
--             then uniqueKey usedKeys
--             else return k

-- genBSTreeInt :: Int -> Gen (BSTree Int String)
-- genBSTreeInt n = genBSTree' n []
--   where
--     genBSTree' :: Int -> [Int] -> Gen (BSTree Int String)
--     genBSTree' 0 _ = return Empty
--     genBSTree' n usedKeys = frequency [
--         (1, return Empty),
--         (6, do 
--             k <- uniqueKey usedKeys
--             v <- resize 5 $ listOf $ elements ['a'..'z']
--             let newUsedKeys = k : usedKeys
--             l <- genBSTree' (n `div` 2) newUsedKeys
--             r <- genBSTree' (n `div` 2) newUsedKeys
--             return $ Node k v l r)]

--     uniqueKey :: [Int] -> Gen Int
--     uniqueKey usedKeys = do
--         k <- arbitrary
--         if k `elem` usedKeys
--             then uniqueKey usedKeys --addd
--             else return k

-- genBSTreeString :: Gen (BSTree String String)
-- genBSTreeString = genBSTree' []
--   where
--     genBSTree' :: [String] -> Gen (BSTree String String)
--     genBSTree' usedKeys = frequency [
--         (1, return Empty),
--         (6, do 
--             k <- uniqueKey usedKeys
--             v <- resize 5 $ listOf $ elements ['a'..'z']
--             let newUsedKeys = k : usedKeys
--             l <- genBSTree' newUsedKeys
--             r <- genBSTree' newUsedKeys
--             return $ Node k v l r)]

--     uniqueKey :: [String] -> Gen String
--     uniqueKey usedKeys = do
--         k <- resize 5 $ listOf $ elements ['a'..'z']
--         if k `elem` usedKeys
--             then uniqueKey usedKeys
--             else return k
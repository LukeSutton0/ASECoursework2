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
-- Generate binary search trees
genBSTree :: Gen (BSTree Int String)
genBSTree = do
  frequency [
      (1, return Empty),
      (4, do 
          k <- arbitrary
          v <- arbitrary
          l <- genBSTree
          r <- genBSTree
          return $ Node k v l r)]

testInsertIntoBSTreeExists :: Property
testInsertIntoBSTreeExists =
  forAll genBSTree $ \t -> --random tree
    forAll arbitrary $ \k -> --random key
      forAll arbitrary $ \v -> --random value
        let newT = insertIntoBSTree k v t --all put together
        in existsInBSTree k newT --testing function vs newT

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
      Empty -> property True -- skip if tree is empty
      _ -> --for any other tree
        let kvs = getKeyValuePairs t
        in forAll (elements kvs) $ \(k, v) ->
             let mval = lookupBSTree k t
             in counterexample (show (k, v, mval)) (mval == Just v)

getKeyValuePairs :: BSTree k v -> [(k, v)]
getKeyValuePairs Empty = []
getKeyValuePairs (Node k v l r) = getKeyValuePairs l ++ [(k, v)] ++ getKeyValuePairs r


bSTreeMain :: IO ()
bSTreeMain = do
  _ <- runTestTT binarySearchTreeTests
  quickCheck testInsertIntoBSTreeExists
  quickCheck testLookupBSTree
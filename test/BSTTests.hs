module BSTTests (binarySearchTreeTests,binarySearchTreeQuickTests) where
import Test.HUnit
import Test.QuickCheck
import BSTree

-- Test cases for `binary Search Tree Tests` 
binarySearchTreeTests :: Test
binarySearchTreeTests = TestList [
  testCreateEmptyBSTree,
  testInsertIntoBSTree
  ]

binarySearchTreeQuickTests :: Property
binarySearchTreeQuickTests = testInsertIntoBSTreeExists


testCreateEmptyBSTree :: Test
testCreateEmptyBSTree = TestCase $ do
  let aTree = createEmptyBSTree :: BSTree Int String
  assertEqual "createEmptyBSTree, should return an empty dict" createEmptyBSTree aTree

testInsertIntoBSTree :: Test
testInsertIntoBSTree = TestCase $ do
    let aTreeWithANode = insertIntoBSTree 1 "My Value for the inserted node" createEmptyBSTree :: BSTree Int String
    assertEqual "insertIntoBSTree, should return a tree with one node" 
      (Node 1 "My Value for the inserted node" Empty Empty) aTreeWithANode



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
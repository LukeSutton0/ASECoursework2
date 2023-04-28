module DictionaryTests (dictionaryMain) where
import Test.HUnit
import Test.QuickCheck
import Debug.Trace
import Data.List
import Control.Monad
import Data.Foldable (Foldable(toList))
import Data.Maybe (isNothing)
import Dictionary
import BSTree (createEmptyBSTree)

-- Test cases for `dictionary Tests`
dictionaryTests :: Test
dictionaryTests = TestList [
  
  ]


-- QuickCheck tests for `dictionary Tests`
-- Generate dictionaries with at most `n` key-value pairs

-- Generate a random dictionary with unique keys
instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Dictionary k v) where
  arbitrary = sized genDict

genDict :: (Arbitrary k, Arbitrary v, Ord k) => Int -> Gen (Dictionary k v)
genDict n  = do
    key <- genUniqueKeys n
    foldM (\dict k -> do
      v <- arbitrary
      return $ insertIntoDict k v dict) createEmptyDict key

genUniqueKeys :: (Arbitrary k, Eq k) => Int -> Gen [k]
genUniqueKeys n = fmap(take n . nub)(infiniteListOf arbitrary)


prop_createEmptyDict :: Bool
prop_createEmptyDict = case createEmptyDict of
                        createEmptyDict -> True
                        _ -> False


-- isEmptyDict :: Dict k v -> Bool
-- isEmptyDict d = sizeDict d == 0


prop_insertIntoDict :: (Eq k, Eq v, Ord k) => k -> v -> Dictionary k v -> Bool
prop_insertIntoDict k v d =
  let updatedDict = insertIntoDict k v d
  in lookupDict k updatedDict == Just v

prop_lookupDict :: Dictionary Int String -> Bool
prop_lookupDict d =
  let keyValues = listDictVals d
  in case keyValues of
       [] -> lookupDict 0 d == Nothing
       _  -> let (k, v) = head keyValues
                 lookupResult = lookupDict k d
             in case lookupResult of
                  Nothing -> False
                  Just value -> value == v




-- instance (Show k, Show v) => Show (Dictionary k v) where
--   show createEmptyBSTree = "Empty"
--   show (Node k v l r) = "Node (" ++ show k ++ ", " ++ show v ++ ") (" ++ show l ++ ") (" ++ show r ++ ")"


dictionaryMain :: IO ()
dictionaryMain = do 
  quickCheck prop_createEmptyDict
  quickCheck prop_lookupDict
  quickCheck (prop_insertIntoDict :: Int -> String -> Dictionary Int String -> Bool)



  --quickCheck (prop_insertIntoDict :: Int -> String -> Dictionary Int String -> Bool)
  --_ <- runTestTT dictionaryTests --run all HUnit tests
  --quickCheck (prop_insertIntoDict :: Int -> String -> Dictionary Int String -> Bool)
  -- quickCheck prop_lookupDict
  -- quickCheck prop_listDictVals
  -- quickCheck (prop_createEmptyDict :: Dict Int String -> Int -> String -> Bool)
  -- quickCheck (prop_removeFromDict :: Int -> Dict Int String -> Bool)


-- prop_listDictVals :: Dictionary Int String -> Bool
-- prop_listDictVals d =
--   let keyValues = toListDict d
--   in all (\(k, v) -> lookupDict k d == Just v) keyValues

-- memberDict :: Ord k => k -> Dict k v -> Bool
-- memberDict k createEmptyDict = False
-- memberDict k (DictNode k' _ l r)
--   | k == k' = True
--   | k < k' = memberDict k l
--   | otherwise = memberDict k r

-- toListDict :: Dict k v -> [(k, v)]
-- toListDict createEmptyDict = []
-- toListDict (DictNode k v l r) = toListDict l ++ [(k, v)] ++ toListDict r


-- prop_removeEntriesIfDict :: Int -> Dictionary Int String -> Bool
-- prop_removeEntriesIfDict p dict = all (\(k, v) -> not (p k) || not (memberDict k result)) pairs
--   where pairs = toListDict dict
--         result = removeEntriesIfDict p dict

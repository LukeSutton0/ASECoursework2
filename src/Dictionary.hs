{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Dictionary (
    createEmptyDict,
    insertIntoDict,
    -- lookupDict,
    -- listDictVals,
    -- removeFromDict,
    -- removeEntriesIfDict,
    Dictionary(..)
) where

import BSTree

-- how to create empty and node for dict

data Dictionary k v = Empty | DictNode (BSTree k v) deriving (Eq, Show)


createEmptyDict :: Dictionary k v
createEmptyDict = Dictionary.Empty

insertIntoDict :: Ord k => k -> v -> Dictionary k v -> Dictionary k v
insertIntoDict k v (DictNode tree) = DictNode (insertIntoBSTree k v tree)

-- lookupDict :: Ord k => k -> Dictionary k v -> Maybe v
-- lookupDict k (DictNode (BSTree.Node leftTree key value rightTree))
--   | k < key   = lookupBSTree k leftTree
--   | k > key   = lookupBSTree k rightTree
--   | otherwise = Just value

-- listDictVals :: Ord k => Dictionary k v -> [(k, v)]
-- listDictVals (DictNode tree) = BSTree.listBSTreeVals tree

-- removeFromDict :: Ord k => k -> Dictionary k v -> Dictionary k v
-- removeFromDict k (DictNode tree) = DictNode $ BSTree.removeFromBSTree k tree

-- removeEntriesIfDict :: Ord k => (k -> v -> Bool) -> Dictionary k v -> Dictionary k v
-- removeEntriesIfDict p (DictNode tree) = DictNode $ BSTree.removeEntriesIf p tree
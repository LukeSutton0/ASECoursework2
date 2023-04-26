module Dictionary (
    Dictionary,
    -- emptyDictionary,
    createEmptyDictionary
) where

import qualified BSTree as BST


type Dictionary k v = BST.BSTree k v
createEmptyDictionary :: Ord k => Dictionary k v
createEmptyDictionary = BST.createEmptyBSTree

-- emptyDictionary :: Dictionary k v
-- emptyDictionary = BST.Empty
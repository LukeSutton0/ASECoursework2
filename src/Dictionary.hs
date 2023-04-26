module Dictionary (
    createEmptyDictionary
) where

import qualified BinaryTree


createEmptyDictionary :: Ord k => [(k, v)]
createEmptyDictionary = BinaryTree.createEmptyDictionary
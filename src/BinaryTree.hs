module BinaryTree (
    createEmptyDictionary,
    insertIntoDictionary
) where

createEmptyDictionary :: Ord k => [(k, v)]
createEmptyDictionary = []

insertIntoDictionary :: Ord k => k -> v -> [(k, v)] -> [(k, v)]
insertIntoDictionary key value [] = [(key, value)]
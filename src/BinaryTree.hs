module BinaryTree (
    BinaryTree(..),
    createBinaryTree
) where

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

createBinaryTree :: [a] -> BinaryTree a
createBinaryTree _ = error "not implemented yet"

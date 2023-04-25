module BinaryTree (
    BinaryTree(..),
    createBinaryTree
) where

data BinaryTree x = EmptyTree | Node x (BinaryTree x) (BinaryTree x) deriving (Eq, Show)

createBinaryTree :: Ord x => [x] -> BinaryTree x -- create a binary tree from a list of values
createBinaryTree [] = EmptyTree -- empty list should return empty tree
createBinaryTree [x] = Node x EmptyTree EmptyTree -- tree with one element should return a leaf
createBinaryTree (x:xs) = Node x (createBinaryTree $ filter (<x) xs) (createBinaryTree $ filter (>=x) xs) -- tree with more elements should recursively call sub nodes

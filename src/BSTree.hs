module BSTree (
    createEmptyBSTree,
    insertIntoDictionary,
    BSTree(..)
) where


data BSTree k v = Empty | Node k v (BSTree k v) (BSTree k v) deriving (Eq, Show)

createEmptyBSTree :: Ord k => BSTree k v
createEmptyBSTree = Empty

insertIntoDictionary :: Ord k => k -> v -> BSTree k v -> BSTree k v
insertIntoDictionary key value Empty = Node key value Empty Empty
insertIntoDictionary key value (Node k v left right)
    | key == k = Node key value left right
    | key < k  = Node k v (insertIntoDictionary key value left) right
    | otherwise = Node k v left (insertIntoDictionary key value right)
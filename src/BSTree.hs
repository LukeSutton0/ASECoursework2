module BSTree (
    createEmptyBSTree,
    insertIntoBSTree,
    lookupBSTree,
    BSTree(..)
) where


data BSTree k v = Empty | Node k v (BSTree k v) (BSTree k v) deriving (Eq, Show)

createEmptyBSTree :: Ord k => BSTree k v
createEmptyBSTree = Empty

-- insertIntoBSTree :: (Num k, Ord k) => k -> v -> BSTree k v -> BSTree k v
-- insertIntoBSTree key value Empty = Node 1 value Empty Empty
-- insertIntoBSTree key value (Node k v left right) = Node k v left right

insertIntoBSTree :: Ord k => k -> v -> BSTree k v -> BSTree k v
insertIntoBSTree key value Empty = Node key value Empty Empty
insertIntoBSTree key value (Node k v left right)
    | key == k = Node key value left right
    | key < k  = Node k v (insertIntoBSTree key value left) right
    | otherwise = Node k v left (insertIntoBSTree key value right)



lookupBSTree :: Ord k => k -> BSTree k v -> Maybe v
lookupBSTree _ Empty = Nothing --if key not in tree return nothing
lookupBSTree key (Node k v left right)
    | key == k = Just v
    | key < k  = lookupBSTree key left
    | otherwise = lookupBSTree key right


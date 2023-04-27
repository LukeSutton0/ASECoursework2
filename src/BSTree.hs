module BSTree (
    createEmptyBSTree,
    insertIntoBSTree,
    lookupBSTree,
    listBSTreeVals,
    removeFromBSTree,
    removeEntriesIf,
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
lookupBSTree _ Empty = Nothing
lookupBSTree key (Node k v left right)
    | key == k = Just v
    | key < k  = lookupBSTree key left
    | otherwise = lookupBSTree key right

listBSTreeVals :: (Ord k) => BSTree k v -> [(k, v)]
listBSTreeVals Empty = []
listBSTreeVals (Node k v left right) = sortedLeft ++ [(k, v)] ++ sortedRight
  where
    sortedLeft = listBSTreeVals left
    sortedRight = listBSTreeVals right

removeFromBSTree :: Ord k => k -> BSTree k v -> BSTree k v
removeFromBSTree _ Empty = Empty
removeFromBSTree key (Node k v left right)
    | key == k = removeNode (Node k v left right)
    | key < k  = Node k v (removeFromBSTree key left) right
    | otherwise = Node k v left (removeFromBSTree key right)
  where
    removeNode :: Ord k => BSTree k v -> BSTree k v
    removeNode (Node _ _ Empty r) = r
    removeNode (Node _ _ l Empty) = l
    removeNode (Node _ _ l r) = Node k' v' l (removeFromBSTree k' r)
      where
        (k', v') = findMin r
        findMin :: Ord k => BSTree k v -> (k, v)
        findMin (Node k v Empty _) = (k, v)
        findMin (Node _ _ l _) = findMin l


removeEntriesIf :: Ord k => (k -> v -> Bool) -> BSTree k v -> BSTree k v
removeEntriesIf _ Empty = Empty
removeEntriesIf p (Node k v left right)
    | p k v = removeEntriesIf p (removeFromBSTree k (Node k v left right))
    | otherwise = Node k v (removeEntriesIf p left) (removeEntriesIf p right)
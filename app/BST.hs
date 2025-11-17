module BST
    ( BST(..)
    , Step(..)
    , insert
    , delete
    , search
    , insertSteps
    ) where

-- Core tree type
data BST a = Empty | Node a (BST a) (BST a)
    deriving (Show, Eq)

-- A snapshot for visualization
data Step a = Step
    { stepTree :: BST a
    , stepDesc :: String
    }
    deriving (Show)

------------------------
-- Basic BST operations
------------------------

search :: Ord a => a -> BST a -> Bool
search _ Empty = False
search x (Node v l r)
    | x == v    = True
    | x < v     = search x l
    | otherwise = search x r

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node v l r)
    | x < v     = Node v (insert x l) r
    | x > v     = Node v l (insert x r)
    | otherwise = Node v l r  -- ignore duplicates

delete :: Ord a => a -> BST a -> BST a
delete _ Empty = Empty
delete x (Node v l r)
    | x < v = Node v (delete x l) r
    | x > v = Node v l (delete x r)
    | otherwise = deleteNode l r

deleteNode :: Ord a => BST a -> BST a -> BST a
deleteNode Empty r = r
deleteNode l Empty = l
deleteNode l r     = Node m l (delete m r)
  where m = findMin r

findMin :: Ord a => BST a -> a
findMin Empty = error "findMin called on Empty tree"
findMin (Node v Empty _) = v
findMin (Node _ left _)  = findMin left

-------------------------------------
-- Insert that RETURNS VISUAL STEPS
-------------------------------------

insertSteps :: (Show a, Ord a) => a -> BST a -> [Step a]
insertSteps x = go
  where
    go Empty =
        [Step (Node x Empty Empty) ("Inserted " ++ show x)]
    go (Node v l r)
        | x < v =
            let next = Node v (insert x l) r
            in Step next ("Went left at " ++ show v) : go l
        | x > v =
            let next = Node v l (insert x r)
            in Step next ("Went right at " ++ show v) : go r
        | otherwise =
            [Step (Node v l r) ("Value " ++ show x ++ " already exists")]

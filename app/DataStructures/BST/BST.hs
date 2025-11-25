module DataStructures.BST.BST
    ( BST(..)
    , Step(..)
    , insert
    , delete
    , search
    , insertSteps
    , searchSteps
    , deleteSteps
    ) where

-- Core tree type
data BST a = Empty | Node a (BST a) (BST a)
    deriving (Show, Eq)

-- A snapshot for visualization
data Step a = Step
    { stepTree :: BST a
    , stepDesc :: String
    , stepHighlight :: Maybe a
    }
    deriving (Show)

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
    | otherwise = Node v l r

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

insertSteps :: (Show a, Ord a) => a -> BST a -> [Step a]
insertSteps x tree = goInsert tree tree
  where
    goInsert original Empty =
        [Step (insert x original) ("Inserted " ++ show x) (Just x)]

    goInsert original (Node v l r)
        | x < v =
            Step original ("Comparing " ++ show x ++ " < " ++ show v ++ ", go left") (Just v)
            : goInsert original l
        | x > v =
            Step original ("Comparing " ++ show x ++ " > " ++ show v ++ ", go right") (Just v)
            : goInsert original r
        | otherwise =
            [Step original ("Value " ++ show x ++ " already exists") (Just v)]

searchSteps :: (Show a, Ord a) => a -> BST a -> [Step a]
searchSteps x tree = goSearch tree
  where
    goSearch Empty =
        [Step tree ("Value " ++ show x ++ " not found") Nothing]

    goSearch (Node v l r)
        | x == v =
            [Step tree ("Found " ++ show x ++ "!") (Just v)]
        | x < v =
            Step tree ("Comparing " ++ show x ++ " < " ++ show v ++ ", go left") (Just v)
            : goSearch l
        | otherwise =
            Step tree ("Comparing " ++ show x ++ " > " ++ show v ++ ", go right") (Just v)
            : goSearch r

deleteSteps :: (Show a, Ord a) => a -> BST a -> [Step a]
deleteSteps x tree = goDelete tree tree
  where
    goDelete original Empty =
        [Step original ("Value " ++ show x ++ " not found, nothing to delete") Nothing]

    goDelete original (Node v l r)
        | x < v =
            Step original ("Comparing " ++ show x ++ " < " ++ show v ++ ", go left") (Just v)
            : goDelete original l
        | x > v =
            Step original ("Comparing " ++ show x ++ " > " ++ show v ++ ", go right") (Just v)
            : goDelete original r
        | otherwise =
            let deletedTree = delete x original
            in [ Step original ("Found " ++ show x ++ ", deleting...") (Just v)
               , Step deletedTree (describeDelete v l r) Nothing
               ]

describeDelete :: (Show a, Ord a) => a -> BST a -> BST a -> String
describeDelete v Empty Empty = "Deleted leaf node " ++ show v
describeDelete v Empty _ = "Deleted " ++ show v ++ ", promoted right child"
describeDelete v _ Empty = "Deleted " ++ show v ++ ", promoted left child"
describeDelete v _ r =
    let successor = findMin r
    in "Deleted " ++ show v ++ ", replaced with successor " ++ show successor
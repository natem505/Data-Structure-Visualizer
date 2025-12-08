module DataStructures.AVL.AVL
    ( AVL(..)
    , Step(..)
    , insert
    , delete
    , search
    , insertSteps
    , searchSteps
    , deleteSteps
    , height
    , RotationType(..)
    ) where

-- AVL tree stores height at each node
data AVL a = Empty | Node a Int (AVL a) (AVL a)  -- value, height, left, right
    deriving (Show, Eq)

-- A snapshot for visualization
data Step a = Step
    { stepTree :: AVL a
    , stepDesc :: String
    , stepHighlight :: Maybe a
    , stepRotationType :: Maybe RotationType
    }
    deriving (Show)

data RotationType = LeftRotation | RightRotation | LeftRightRotation | RightLeftRotation
    deriving (Show, Eq)

-- Get height of tree
height :: AVL a -> Int
height Empty = 0
height (Node _ h _ _) = h

-- Get balance factor (left height - right height)
balanceFactor :: AVL a -> Int
balanceFactor Empty = 0
balanceFactor (Node _ _ l r) = height l - height r

-- Create node with correct height
node :: a -> AVL a -> AVL a -> AVL a
node v l r = Node v (1 + max (height l) (height r)) l r

-- Search (same as BST)
search :: Ord a => a -> AVL a -> Bool
search _ Empty = False
search x (Node v _ l r)
    | x == v    = True
    | x < v     = search x l
    | otherwise = search x r

-- Rotations
rotateRight :: AVL a -> AVL a
rotateRight (Node v _ (Node lv _ ll lr) r) = node lv ll (node v lr r)
rotateRight t = t

rotateLeft :: AVL a -> AVL a
rotateLeft (Node v _ l (Node rv _ rl rr)) = node rv (node v l rl) rr
rotateLeft t = t

-- Balance the tree after insertion/deletion
balance :: AVL a -> AVL a
balance t@(Node v _ l r)
    | balanceFactor t > 1 && balanceFactor l >= 0 = rotateRight t  -- Left-Left
    | balanceFactor t > 1 && balanceFactor l < 0 = rotateRight (node v (rotateLeft l) r)  -- Left-Right
    | balanceFactor t < -1 && balanceFactor r <= 0 = rotateLeft t  -- Right-Right
    | balanceFactor t < -1 && balanceFactor r > 0 = rotateLeft (node v l (rotateRight r))  -- Right-Left
    | otherwise = t
balance Empty = Empty

-- Insert with balancing
insert :: Ord a => a -> AVL a -> AVL a
insert x Empty = node x Empty Empty
insert x (Node v _ l r)
    | x < v     = balance (node v (insert x l) r)
    | x > v     = balance (node v l (insert x r))
    | otherwise = Node v (height (Node v 0 l r)) l r

-- Find minimum
findMin :: AVL a -> a
findMin Empty = error "findMin on Empty"
findMin (Node v _ Empty _) = v
findMin (Node _ _ l _) = findMin l

-- Delete with balancing
delete :: Ord a => a -> AVL a -> AVL a
delete _ Empty = Empty
delete x (Node v _ l r)
    | x < v = balance (node v (delete x l) r)
    | x > v = balance (node v l (delete x r))
    | otherwise = deleteNode l r
  where
    deleteNode Empty r' = r'
    deleteNode l' Empty = l'
    deleteNode l' r' = balance (node (findMin r') l' (delete (findMin r') r'))

-- Insert with steps for visualization
insertSteps :: (Show a, Ord a) => a -> AVL a -> [Step a]
insertSteps x tree =
    let searchSteps = goSearch tree tree
        unbalancedTree = insertUnbalanced x tree
        finalTree = insert x tree
        needsRebalance = not (sameStructure unbalancedTree finalTree)
    in if needsRebalance
       then searchSteps ++
            [ Step unbalancedTree ("Inserted " ++ show x ++ " - tree is UNBALANCED!") (Just x) Nothing
            , Step unbalancedTree ("Balance factor violation detected") Nothing Nothing
            , Step finalTree ("Performing rotation to rebalance...") Nothing (detectRotationType unbalancedTree finalTree)
            , Step finalTree ("Tree rebalanced!") Nothing Nothing
            ]
       else searchSteps ++ [Step finalTree ("Inserted " ++ show x) (Just x) Nothing]
  where
    goSearch original Empty = []
    goSearch original (Node v _ l r)
        | x < v = Step original ("Comparing " ++ show x ++ " < " ++ show v ++ ", go left") (Just v) Nothing : goSearch original l
        | x > v = Step original ("Comparing " ++ show x ++ " > " ++ show v ++ ", go right") (Just v) Nothing : goSearch original r
        | otherwise = [Step original ("Value " ++ show x ++ " already exists") (Just v) Nothing]

-- Insert without any balancing
insertUnbalanced :: Ord a => a -> AVL a -> AVL a
insertUnbalanced x Empty = node x Empty Empty
insertUnbalanced x (Node v _ l r)
    | x < v     = node v (insertUnbalanced x l) r
    | x > v     = node v l (insertUnbalanced x r)
    | otherwise = Node v (height (Node v 0 l r)) l r

-- Check if two trees have same structure
sameStructure :: Eq a => AVL a -> AVL a -> Bool
sameStructure Empty Empty = True
sameStructure (Node v1 _ l1 r1) (Node v2 _ l2 r2) =
    v1 == v2 && sameStructure l1 l2 && sameStructure r1 r2
sameStructure _ _ = False

detectRotationType :: AVL a -> AVL a -> Maybe RotationType
detectRotationType _ _ = Just LeftRotation  -- Simplified for now

-- Search steps (same as BST)
searchSteps :: (Show a, Ord a) => a -> AVL a -> [Step a]
searchSteps x tree = go tree
  where
    go Empty =
        [Step tree ("Value " ++ show x ++ " not found") Nothing Nothing]

    go (Node v _ l r)
        | x == v =
            [Step tree ("Found " ++ show x ++ "!") (Just v) Nothing]
        | x < v =
            Step tree ("Comparing " ++ show x ++ " < " ++ show v ++ ", go left") (Just v) Nothing : go l
        | otherwise =
            Step tree ("Comparing " ++ show x ++ " > " ++ show v ++ ", go right") (Just v) Nothing : go r

-- Delete steps
deleteSteps :: (Show a, Ord a) => a -> AVL a -> [Step a]
deleteSteps x tree =
    let searchSteps = goSearch tree tree
        finalTree = delete x tree
        -- Check if tree exists after deletion (wasn't empty and value was found)
        treeChanged = tree /= finalTree
    in if treeChanged
       then
           let -- Create intermediate tree that shows deletion before rebalancing
               deletedUnbalanced = deleteUnbalanced x tree
               needsRebalance = not (sameStructure deletedUnbalanced finalTree)
           in if needsRebalance
              then searchSteps ++
                   [ Step tree ("Found " ++ show x ++ ", deleting...") (Just x) Nothing
                   , Step deletedUnbalanced ("Deleted " ++ show x ++ " - tree is UNBALANCED!") Nothing Nothing
                   , Step deletedUnbalanced ("Balance factor violation detected") Nothing Nothing
                   , Step finalTree ("Performing rotation to rebalance...") Nothing (detectRotationType deletedUnbalanced finalTree)
                   , Step finalTree ("Tree rebalanced!") Nothing Nothing
                   ]
              else searchSteps ++
                   [ Step tree ("Found " ++ show x ++ ", deleting...") (Just x) Nothing
                   , Step finalTree ("Deleted " ++ show x) Nothing Nothing
                   ]
       else [Step tree ("Value " ++ show x ++ " not found, nothing to delete") Nothing Nothing]
  where
    goSearch original Empty = []
    goSearch original (Node v _ l r)
        | x < v = Step original ("Comparing " ++ show x ++ " < " ++ show v ++ ", go left") (Just v) Nothing : goSearch original l
        | x > v = Step original ("Comparing " ++ show x ++ " > " ++ show v ++ ", go right") (Just v) Nothing : goSearch original r
        | otherwise = []  -- Found it, stop searching

-- Delete without balancing (for visualization)
deleteUnbalanced :: Ord a => a -> AVL a -> AVL a
deleteUnbalanced _ Empty = Empty
deleteUnbalanced x (Node v _ l r)
    | x < v = node v (deleteUnbalanced x l) r
    | x > v = node v l (deleteUnbalanced x r)
    | otherwise = deleteNodeUnbalanced l r
  where
    deleteNodeUnbalanced Empty r' = r'
    deleteNodeUnbalanced l' Empty = l'
    deleteNodeUnbalanced l' r' = node (findMin r') l' (deleteUnbalanced (findMin r') r')

describeDelete :: (Show a, Ord a) => a -> AVL a -> AVL a -> String
describeDelete v Empty Empty = "Deleted leaf node " ++ show v
describeDelete v Empty _ = "Deleted " ++ show v ++ ", promoted right child"
describeDelete v _ Empty = "Deleted " ++ show v ++ ", promoted left child"
describeDelete v _ r =
    let successor = findMin r
    in "Deleted " ++ show v ++ ", replaced with successor " ++ show successor
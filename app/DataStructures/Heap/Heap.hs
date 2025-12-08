module DataStructures.Heap.Heap
    ( Heap(..)
    , Step(..)
    , empty
    , insert
    , extractMax
    , peek
    , fromList
    , toList
    , insertSteps
    , extractSteps
    , heapSortSteps
    , heapify
    , leftChildIndex
    , rightChildIndex
    , parentIndex
    ) where

import Data.List (foldl')

-- Max-Heap: parent is always larger than children
-- Stored as array but visualized as tree
data Heap a = Heap [a]
    deriving (Show, Eq)

-- A snapshot for visualization
data Step a = Step
    { stepHeap :: Heap a
    , stepDesc :: String
    , stepHighlight :: Maybe Int
    , stepCompareIndices :: Maybe (Int, Int)
    , stepSortedArray :: Maybe [a]  -- For heapsort visualization
    }
    deriving (Show)

empty :: Heap a
empty = Heap []

-- Get element at index (0-based)
getAt :: [a] -> Int -> Maybe a
getAt xs i
    | i < 0 || i >= length xs = Nothing
    | otherwise = Just (xs !! i)

-- Parent/child index calculations
parentIndex :: Int -> Int
parentIndex i = (i - 1) `div` 2

leftChildIndex :: Int -> Int
leftChildIndex i = 2 * i + 1

rightChildIndex :: Int -> Int
rightChildIndex i = 2 * i + 2

-- Peek at maximum element
peek :: Heap a -> Maybe a
peek (Heap []) = Nothing
peek (Heap (x:_)) = Just x

-- Convert heap to list
toList :: Heap a -> [a]
toList (Heap xs) = xs

-- Build heap from list
fromList :: Ord a => [a] -> Heap a
fromList xs = foldl' (\h x -> insert x h) empty xs

-- Insert element
insert :: Ord a => a -> Heap a -> Heap a
insert x (Heap xs) = Heap (bubbleUp (xs ++ [x]) (length xs))

-- Bubble up to maintain heap property (MAX heap)
bubbleUp :: Ord a => [a] -> Int -> [a]
bubbleUp xs i
    | i == 0 = xs
    | otherwise =
        let pi = parentIndex i
            Just parent = getAt xs pi
            Just current = getAt xs i
        in if current > parent  -- Changed from < to > for MAX heap
           then bubbleUp (swap xs i pi) pi
           else xs

-- Extract maximum element
extractMax :: Ord a => Heap a -> Maybe (a, Heap a)
extractMax (Heap []) = Nothing
extractMax (Heap [x]) = Just (x, Heap [])
extractMax (Heap (x:xs)) =
    let lastIdx = length xs
        newHeap = bubbleDown (last xs : init xs) 0
    in Just (x, Heap newHeap)

-- Bubble down to maintain heap property (MAX heap)
bubbleDown :: Ord a => [a] -> Int -> [a]
bubbleDown xs i
    | leftChildIndex i >= length xs = xs
    | otherwise =
        let li = leftChildIndex i
            ri = rightChildIndex i
            Just current = getAt xs i
            Just leftChild = getAt xs li
            rightChild = getAt xs ri

            -- Find largest among current, left, right (changed from smallest)
            largestIdx = case rightChild of
                Nothing -> if leftChild > current then li else i
                Just rc -> if leftChild >= rc && leftChild > current
                          then li
                          else if rc > current then ri else i
        in if largestIdx /= i
           then bubbleDown (swap xs i largestIdx) largestIdx
           else xs

-- Swap two elements in list
swap :: [a] -> Int -> Int -> [a]
swap xs i j =
    let elemI = xs !! i
        elemJ = xs !! j
        updateAt idx val list = take idx list ++ [val] ++ drop (idx + 1) list
        temp = updateAt i elemJ xs
    in updateAt j elemI temp

-- Heapify - convert array to heap
heapify :: Ord a => [a] -> Heap a
heapify xs = Heap (heapifyArray xs)

heapifyArray :: Ord a => [a] -> [a]
heapifyArray xs = foldl' bubbleDown xs [lastParent, lastParent - 1 .. 0]
  where
    lastParent = parentIndex (length xs - 1)

-- Insert with animation steps
insertSteps :: (Show a, Ord a) => a -> Heap a -> [Step a]
insertSteps x (Heap xs) =
    let newHeap = xs ++ [x]
        startIdx = length xs
    in Step (Heap newHeap) ("Inserted " ++ show x ++ " at end of array") (Just startIdx) Nothing Nothing
       : bubbleUpSteps newHeap startIdx

bubbleUpSteps :: (Show a, Ord a) => [a] -> Int -> [Step a]
bubbleUpSteps xs i
    | i == 0 = [Step (Heap xs) "Reached root - heap property satisfied" (Just i) Nothing Nothing]
    | otherwise =
        let pi = parentIndex i
            Just parent = getAt xs pi
            Just current = getAt xs i
        in if current > parent  -- Changed from < to > for MAX heap
           then Step (Heap xs)
                    ("Comparing " ++ show current ++ " > " ++ show parent ++ ", swap with parent")
                    (Just i)
                    (Just (i, pi))
                    Nothing
                : bubbleUpSteps (swap xs i pi) pi
           else [Step (Heap xs) "Heap property satisfied" (Just i) Nothing Nothing]

-- Extract with animation steps
extractSteps :: (Show a, Ord a) => Heap a -> [Step a]
extractSteps (Heap []) = [Step (Heap []) "Heap is empty" Nothing Nothing Nothing]
extractSteps (Heap [x]) =
    [ Step (Heap [x]) ("Extracting maximum: " ++ show x) (Just 0) Nothing Nothing
    , Step (Heap []) "Heap is now empty" Nothing Nothing Nothing
    ]
extractSteps (Heap (x:xs)) =
    let lastIdx = length xs
        maxVal = x
        lastVal = last xs
        newHeap = lastVal : init xs
    in Step (Heap (x:xs)) ("Extracting maximum: " ++ show maxVal) (Just 0) Nothing Nothing
       : Step (Heap newHeap) ("Moved last element to root: " ++ show lastVal) (Just 0) Nothing Nothing
       : bubbleDownSteps newHeap 0

bubbleDownSteps :: (Show a, Ord a) => [a] -> Int -> [Step a]
bubbleDownSteps xs i
    | leftChildIndex i >= length xs =
        [Step (Heap xs) "Reached leaf - heap property satisfied" (Just i) Nothing Nothing]
    | otherwise =
        let li = leftChildIndex i
            ri = rightChildIndex i
            Just current = getAt xs i
            Just leftChild = getAt xs li
            rightChild = getAt xs ri

            largestIdx = case rightChild of
                Nothing -> if leftChild > current then li else i
                Just rc -> if leftChild >= rc && leftChild > current
                          then li
                          else if rc > current then ri else i
        in if largestIdx /= i
           then let Just largest = getAt xs largestIdx
                in Step (Heap xs)
                       ("Comparing " ++ show current ++ " < " ++ show largest ++ ", swap with child")
                       (Just i)
                       (Just (i, largestIdx))
                       Nothing
                   : bubbleDownSteps (swap xs i largestIdx) largestIdx
           else [Step (Heap xs) "Heap property satisfied" (Just i) Nothing Nothing]

-- HeapSort: Sort array in ascending order using max heap
heapSortSteps :: (Show a, Ord a) => [a] -> [Step a]
heapSortSteps [] = [Step (Heap []) "Empty array - nothing to sort" Nothing Nothing (Just [])]
heapSortSteps xs =
    let -- Step 1: Build max heap
        heapified = heapifyArray xs
        buildSteps = [Step (Heap heapified) "Built max heap from array" Nothing Nothing Nothing]

        -- Step 2: Extract max repeatedly and build sorted array
        sortSteps = heapSortExtract heapified []
    in buildSteps ++ sortSteps

heapSortExtract :: (Show a, Ord a) => [a] -> [a] -> [Step a]
heapSortExtract [] sorted = [Step (Heap []) "Sorting complete!" Nothing Nothing (Just sorted)]
heapSortExtract [x] sorted =
    let finalSorted = x : sorted
    in [Step (Heap [x]) ("Last element: " ++ show x) (Just 0) Nothing (Just sorted)
       , Step (Heap []) "Sorting complete!" Nothing Nothing (Just finalSorted)
       ]
heapSortExtract heap sorted =
    let maxVal = head heap
        lastVal = last heap
        newHeapUnsorted = lastVal : init (tail heap)
        newSorted = maxVal : sorted
        steps1 = [ Step (Heap heap) ("Extract max: " ++ show maxVal) (Just 0) Nothing (Just sorted)
                 , Step (Heap newHeapUnsorted) ("Move last to root, reheapify") (Just 0) Nothing (Just newSorted)
                 ]
        newHeap = bubbleDown newHeapUnsorted 0
        steps2 = [Step (Heap newHeap) "Heap property restored" Nothing Nothing (Just newSorted)]
    in steps1 ++ steps2 ++ heapSortExtract newHeap newSorted
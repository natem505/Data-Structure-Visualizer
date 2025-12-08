module DataStructures.HashTable.HashTable
    ( HashTable
    , Step(..)
    , empty
    , insert
    , delete
    , search
    , insertSteps
    , searchSteps
    , deleteSteps
    , getBuckets
    , getSize
    ) where

import qualified Data.List as List

-- Hash table with separate chaining (each bucket is a list)
data HashTable a = HashTable
    { htSize :: Int
    , htBuckets :: [[a]]
    }
    deriving (Show, Eq)

-- A snapshot for visualization
data Step a = Step
    { stepTable :: HashTable a
    , stepDesc :: String
    , stepHighlightIndex :: Maybe Int
    , stepHighlightValue :: Maybe a
    , stepHashCalc :: Maybe String
    }
    deriving (Show)

-- Create empty hash table
empty :: Int -> HashTable a
empty size = HashTable size (replicate size [])

-- Get buckets
getBuckets :: HashTable a -> [[a]]
getBuckets = htBuckets

-- Get size
getSize :: HashTable a -> Int
getSize = htSize

-- Hash function (simple modulo)
hash :: Int -> Int -> Int
hash value size = value `mod` size

-- Insert value
insert :: Int -> HashTable Int -> HashTable Int
insert val (HashTable size buckets) =
    let idx = hash val size
        bucket = buckets !! idx
        newBucket = if val `List.elem` bucket then bucket else val : bucket
        newBuckets = take idx buckets ++ [newBucket] ++ drop (idx + 1) buckets
    in HashTable size newBuckets

-- Search for value
search :: Int -> HashTable Int -> Bool
search val (HashTable size buckets) =
    let idx = hash val size
        bucket = buckets !! idx
    in val `List.elem` bucket

-- Delete value
delete :: Int -> HashTable Int -> HashTable Int
delete val (HashTable size buckets) =
    let idx = hash val size
        bucket = buckets !! idx
        newBucket = List.delete val bucket
        newBuckets = take idx buckets ++ [newBucket] ++ drop (idx + 1) buckets
    in HashTable size newBuckets

-- Insert with animation steps
insertSteps :: Int -> HashTable Int -> [Step Int]
insertSteps val table@(HashTable size buckets) =
    let idx = hash val size
        bucket = buckets !! idx
        hashCalc = "hash(" ++ show val ++ ") = " ++ show val ++ " mod " ++ show size ++ " = " ++ show idx
    in [ Step table ("Calculating hash for " ++ show val) Nothing (Just val) (Just hashCalc)
       , Step table ("Hash index: " ++ show idx) (Just idx) (Just val) (Just hashCalc)
       , Step table ("Checking bucket " ++ show idx) (Just idx) Nothing Nothing
       ] ++
       (if val `List.elem` bucket
        then [Step table ("Value " ++ show val ++ " already exists in bucket " ++ show idx) (Just idx) (Just val) Nothing]
        else [ Step table ("Inserting " ++ show val ++ " into bucket " ++ show idx) (Just idx) (Just val) Nothing
             , Step (insert val table) ("Inserted " ++ show val) (Just idx) (Just val) Nothing
             ])

-- Search with animation steps
searchSteps :: Int -> HashTable Int -> [Step Int]
searchSteps val table@(HashTable size buckets) =
    let idx = hash val size
        bucket = buckets !! idx
        hashCalc = "hash(" ++ show val ++ ") = " ++ show val ++ " mod " ++ show size ++ " = " ++ show idx
        found = val `List.elem` bucket
    in [ Step table ("Calculating hash for " ++ show val) Nothing (Just val) (Just hashCalc)
       , Step table ("Hash index: " ++ show idx) (Just idx) (Just val) (Just hashCalc)
       , Step table ("Searching bucket " ++ show idx) (Just idx) Nothing Nothing
       ] ++
       (if found
        then [Step table ("Found " ++ show val ++ " in bucket " ++ show idx) (Just idx) (Just val) Nothing]
        else [Step table ("Value " ++ show val ++ " not found in bucket " ++ show idx) (Just idx) Nothing Nothing])

-- Delete with animation steps
deleteSteps :: Int -> HashTable Int -> [Step Int]
deleteSteps val table@(HashTable size buckets) =
    let idx = hash val size
        bucket = buckets !! idx
        hashCalc = "hash(" ++ show val ++ ") = " ++ show val ++ " mod " ++ show size ++ " = " ++ show idx
        found = val `List.elem` bucket
    in [ Step table ("Calculating hash for " ++ show val) Nothing (Just val) (Just hashCalc)
       , Step table ("Hash index: " ++ show idx) (Just idx) (Just val) (Just hashCalc)
       , Step table ("Searching bucket " ++ show idx) (Just idx) Nothing Nothing
       ] ++
       (if found
        then [ Step table ("Found " ++ show val ++ " in bucket " ++ show idx) (Just idx) (Just val) Nothing
             , Step (delete val table) ("Deleted " ++ show val ++ " from bucket " ++ show idx) (Just idx) Nothing Nothing
             ]
        else [Step table ("Value " ++ show val ++ " not found, nothing to delete") (Just idx) Nothing Nothing])
module BTrees where

import Lib
import Control.Monad.Logic


data BTree = BTree [Int] [BTree] deriving Show

minChildSize :: Int -> Int 
minChildSize h = 2 * (2 ^ (h-1) - 1)

maxChildSize :: Int -> Int
maxChildSize h = (4 ^ h) - 1

getAdditions :: Int -> Int -> Int -> Logic [Int]
getAdditions 0 amount _
    | amount > 0    = mzero
    | otherwise     = return []
getAdditions 1 amount max
    | amount > max  = mzero
getAdditions size amount max = do
    added <- inInclusiveRange 0 rangeEnd
    rest <- getAdditions (size -1) (amount - added) max
    return $ added : rest
    where
        rangeEnd = min amount max

-- sublist :: [a] -> [a]
-- sublist [] = []
-- sublist (h:t) = h : sublist t
-- sublist (h:t) = sublist t

sublistOfSize :: [Int] -> Int -> Logic [Int]
sublistOfSize = undefined

rootBTreeHeight :: Int -> Logic Int
rootBTreeHeight size = inInclusiveRange 1 (logInt (size + 1) + 1)

makeBTree :: Int -> Logic BTree
makeBTree size = do
    height <- rootBTreeHeight size
    makeRootBTree size 1 size height

makeRootBTree :: Int -> Int -> Int -> Int -> Logic BTree
makeRootBTree size start end height
    | height == 1 = do
        guard (size < 4)
        keyRangeList <- inInclusiveRange start end
        x <- sublistOfSize [keyRangeList] size
        return $ BTree x []


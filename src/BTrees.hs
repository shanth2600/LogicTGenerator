module BTrees where

import Lib
import Data.List
import Control.Monad.Logic


data BTree = BTree [Int] [BTree] deriving Show

minChildSize :: Int -> Int 
minChildSize h = 2 * (2 ^ (h-1)) - 1

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

sublist :: [a] -> Logic [a]
sublist [] = return []
sublist (h:t) = do
    rest <- sublist t
    (h:rest) `liftOr` rest

liftOr :: a -> a -> Logic a
liftOr  a1 a2 = (return a1) `mplus` (return a2)
    

sublistOfSize :: Eq a => [a] -> Int -> Logic [a]
sublistOfSize input size = do
    subl <- sublist input
    guard (length subl == size)
    return $ subl

rootBTreeHeight :: Int -> Logic Int
rootBTreeHeight size = inInclusiveRange 1 (logInt (size + 1) + 1)

bTreeDriver :: Int -> Logic BTree
bTreeDriver size = do
    height <- rootBTreeHeight size
    makeRootBTree size 1 size height

makeRootBTree :: Int -> Int -> Int -> Int -> Logic BTree
makeRootBTree size start end height
    | height == 1 = do
        guard (size < 4)
        keyRangeList <- inInclusiveRange start end
        x <- sublistOfSize [keyRangeList] size
        return $ BTree x []
    | height > 1 = do
        x <- inInclusiveRange 2 4
        makeBTree x size start end height

makeBTree :: Int -> Int -> Int -> Int -> Int -> Logic BTree
makeBTree nChildren size start end height = do
    minChildSizeBelow <- return $ minChildSize (height - 1)
    maxChildSizeBelow <- return $ maxChildSize (height - 1)
    addListMax <- return $ maxChildSizeBelow - minChildSizeBelow
    restOfNodes <- return $ size - (nChildren - 1) - nChildren * minChildSizeBelow
    keyRangeSize <- return $ end - start + 1
    addKeysAmount <- return $ keyRangeSize - size
    guard (restOfNodes >= 0)
    addList <- getAdditions nChildren restOfNodes addListMax
    childSizes <- return $ map (\c -> c + minChildSizeBelow) addList
    addKeys <- getAdditions nChildren addKeysAmount keyRangeSize
    keys <- return $ keysHelper childSizes addKeys start
    childRanges <- return $ childRangesHelper start keys end
    children <- childrenHelper childSizes childRanges (height - 1)
    return $ BTree keys children

makeNonRootBTree :: Int -> Int -> Int -> Int -> Logic BTree
makeNonRootBTree size start end height
    | height == 1 = do
        guard (size < 4)
        guard (size >= 1)
        keyRangeList <- inInclusiveRange start end
        x <- sublistOfSize [keyRangeList] size
        return $ BTree x []
    | otherwise = do
        guard (height > 0)
        guard (size > 0)
        x <- inInclusiveRange start end
        makeBTree x size start end height

keysHelper :: [Int] -> [Int] -> Int -> [Int]
keysHelper childSizes addKeys start =
    init tailResult
    where 
        zipResult = zip childSizes addKeys
        (x : tailResult) = scanLeft zipResult (start - 1) (\soFar (childSize, add) -> soFar + childSize + add)

childRangesHelper :: Int -> [Int] -> Int -> [(Int,Int)]
childRangesHelper start keys end =
    map (\(a,b) -> (a + 1, b - 1)) zipResult
    where
        zipRight = keys ++ [(end + 1)]
        zipResult = zip ((start - 1) : keys) zipRight

childrenHelper :: [Int] -> [(Int,Int)] -> Int -> Logic [BTree]
childrenHelper childSizes childRanges hMinusOne = 
        sequence $ map (\(size , (start, end)) -> makeNonRootBTree size start end hMinusOne) zipResult
        where
            zipResult = zip childSizes childRanges



scanLeft :: [a] -> b -> (b -> a -> b) -> [b]
scanLeft [] b _ = [b]
scanLeft (a:as) b r = scanLeft as (r b a) r
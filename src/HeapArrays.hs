module HeapArrays where

import Lib
import Control.Monad.Logic


data Tree a = Node a (Tree a) (Tree a) | Leaf a
data Heap = HeapLeaf | HeapNode Heap Int Heap deriving Show

makeTree :: Int -> Logic (Tree Int)
makeTree size = undefined

makeHeap :: Int -> Int -> Int -> Logic Heap
makeHeap size start end
    | size > (end - start + 1) = mzero
    | size <= 0 = return HeapLeaf

makeHeap 1 start end = do
    i <- until' 0 rangeSize
    return $ HeapNode HeapLeaf (i + end) HeapLeaf
    where
        rangeSize = end - start + 1

makeHeap size start end = do
    guard (end >= start)
    rootInd <- downTo end start
    left <- makeHeap actualLeftSize 0 rootInd
    right <- makeHeap leftSize 0 rootInd
    return $ HeapNode left rootInd right
    where
        leftSize = (size - 1) `div` 2
        actualLeftSize = (size - 1) - leftSize
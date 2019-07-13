module BinarySearchTrees where

import Lib
import Control.Monad.Logic


data Tree = Node Tree Int Tree | Leaf

makeBSTNodeBound :: Int -> Int -> Int -> Logic Tree
makeBSTNodeBound size start end
    | size <= 0 = return Leaf

makeBSTNodeBound 1 start end = do
    i <- inInclusiveRange start end
    return $ Node Leaf i Leaf

makeBSTNodeBound size start end = do
    leftSize <- until' 0 size
    median <- inInclusiveRange (start + leftSize) (end - (size - leftSize - 1))
    left <- return $ makeBSTNodeBound leftSize start (median - 1)
    right <- return $ makeBSTNodeBound (size - leftSize - 1) (median + 1) end
    Node <$> left <*> return median <*> right


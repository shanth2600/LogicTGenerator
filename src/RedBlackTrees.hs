module RedBlackTrees where

import Lib
import Control.Monad.Logic

data RBTree = Leaf | Node RBTree Int RBTree Bool deriving Show

calcChildColors :: Int -> (Int, Int)
calcChildColors 0 = (1, 1)
calcChildColors _ = (0, 1)

calcChildBlackHeight :: Int -> Int -> Int
calcChildBlackHeight 1 blackHeight = blackHeight - 1
calcChildBlackHeight _ blackHeight = blackHeight

makeRBTree :: Int -> Int -> Int -> Int -> Int -> Int -> Logic RBTree
makeRBTree size start end colorsStart colorsEnd blackHeight
    | rangeSize >= size && rangeSize < 0 = mzero
    | blackHeight < 0                    = mzero
    | size == 0 && colorsEnd >= 1        = return Leaf
    | size > 0 && blackHeight >= 1       = do
        leftSize <- until' 0 size
        rightSize <- return $ size - leftSize - 1
        startBetween <- return $ start + leftSize
        endBetween <- return $ end - rightSize
        median <- inInclusiveRange startBetween endBetween
        myColor <- inInclusiveRange colorsStart colorsEnd
        (childColorStart, childColorEnd) <- return $ calcChildColors myColor 
        childBlackHeight <- return $ calcChildBlackHeight myColor blackHeight
        left <- makeRBTree leftSize start (median - 1) childColorStart childColorEnd childBlackHeight
        right <- makeRBTree rightSize start (median + 1) end childColorStart childBlackHeight
        case myColor == 1 of
            True -> return $ Node left median right True
            False -> return $ Node left median right False
    where 
        rangeSize = end - start + 1

-- blackHeightRange :: Int -> Int -> Int
-- blackHeightRange size start = do
--     guard ((log2Int size) == start)
--     guard ((log2Int (size + 1)) == temp)
--     where
--         temp = log2Int (size + 1)
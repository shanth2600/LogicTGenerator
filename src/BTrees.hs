module BTrees where

import Control.Monad.Logic
import Control.Applicative

data BTree = BTree [Int] [BTree]

minChildSize :: Int -> Int 
minChildSize h = 2 * (2 ^ (h-1) - 1)

maxChildSize :: Int -> Int
maxChildSize h = (4 ^ h) - 1

getAdditions :: Int -> Int -> Int -> [Int]
getAdditions 0 amount _
    | amount > 0    = mzero
    | otherwise     = []
getAdditions 1 amount max
    | amount > max  = mzero
-- getAdditions size amount max = do
--     added <- return $ inInclusiveRange 0 rangeEnd
--     getAdditions (size - 1) (amount - added) max
--     where
--         rangeEnd = min amount max



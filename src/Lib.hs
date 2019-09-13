module Lib where

import Control.Monad.Logic
import Control.Applicative

member :: [a] -> Logic a
member [] = mzero
member (a : as) = return a `mplus` member as

inInclusiveRange :: Int -> Int -> Logic Int
inInclusiveRange start end = member $ inInclusiveRangeList start end

inInclusiveRangeList :: Int -> Int -> [Int]
inInclusiveRangeList start end = [start .. end]

until' :: Int -> Int -> Logic Int
until' start end = member $ inInclusiveRangeList start (end - 1)

downTo :: Int -> Int -> Logic Int
downTo start end = member [end .. start]

log2Int :: Int -> Int
log2Int input = floor $ (log (fromIntegral input)) / (log 2)

logInt :: Int -> Int
logInt input = floor $ log (fromIntegral input)

countResults l = runLogic l (\a r  -> 1 + r) 0

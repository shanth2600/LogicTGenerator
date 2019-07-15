module Lib where

import Control.Monad.Logic
import Control.Applicative
import Number.FixedPoint

member :: [a] -> Logic a
member [] = mzero
member (a : as) = return a <|> member as

inInclusiveRange :: Int -> Int -> Logic Int
inInclusiveRange start end = member [start .. end]

until' :: Int -> Int -> Logic Int
until' start end = member [start..(end - 1)]

downTo :: Int -> Int -> Logic Int
downTo start end = member [end .. start]

log2Int :: Int -> Int
log2Int input = round $ (log (fromIntegral input)) / (log 2)

logInt :: Int -> Int
logInt input = round $ log (fromIntegral input)
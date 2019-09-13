module Main where

import System.Environment
import BinarySearchTrees
import RedBlackTrees
import HeapArrays
import BTrees

getModule "bst" = BinarySearchTrees.bstMain
getModule "rbt" = RedBlackTrees.rbMain
getModule "heap" = HeapArrays.heapMain
getModule "bt" = BTrees.bTreeMain

main :: IO ()
main = do
  moduleString : sizeString : [] <- getArgs
  let size = read sizeString :: Int
  putStrLn $ show $ getModule moduleString size

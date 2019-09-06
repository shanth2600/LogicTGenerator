module RiffImages where

import Lib
import Control.Monad.Logic

data RiffChunk = 
      RiffLeaf 
    | RiffPayload Int Int Int 
    | RiffNode Int RiffChunk RiffChunk
        deriving Show

makeRiff :: Int -> Int -> Int -> Int -> Logic RiffChunk
makeRiff 0 0 _ _ = return RiffLeaf
makeRiff size dataSize jiffLoss avChunks 
    | size == 1 = do
        guard (dataSize > 0)
        guard (avChunks <= 1)
        guard ((jiffLoss * 4) `mod` dataSize == 0)
        outputDataSize <- return $ (jiffLoss * 4) `div` dataSize
        return $ RiffPayload dataSize outputDataSize avChunks
    | otherwise = do
        leftSize <- until' 0 (size - 1)
        rightTreeSize <- return $ size - leftSize - 1
        leftAudioStart <- return $ max 0 (avChunks - (size - leftSize - 1))
        leftAudioEnd <- return $ min leftSize avChunks
        leftAudio <- inInclusiveRange leftAudioStart leftAudioEnd
        rightAvChunks <- return $ avChunks - leftAudio
        leftDataSize <- inInclusiveRange 0 (dataSize `div` 2)
        rightDataSize <- return $ dataSize - leftDataSize
        leftJiffStart <- return $ max 0 (jiffLoss - (dataSize - leftDataSize))
        leftJiffEnd <- return $ min leftDataSize jiffLoss
        leftJiff <- inInclusiveRange leftJiffStart leftJiffEnd
        rightJiffLoss <- return $ jiffLoss - leftJiff
        leftTree <- makeRiff leftSize leftDataSize leftJiff leftAudio
        rightTree <- makeRiff rightTreeSize rightDataSize rightJiffLoss rightAvChunks
        return $ RiffNode dataSize leftTree rightTree

runMakeRiff :: Int -> Logic RiffChunk
runMakeRiff size =
    makeRiff size size ((size + 1) `div` 2) (size `div` 2)

riffMain size = countResults (runMakeRiff size)
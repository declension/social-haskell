module Main where

import           System.IO
import           App
import           Data.Time (getCurrentTime)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    ts <- getCurrentTime
    process $ emptyState ts

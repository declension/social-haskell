module Main where

import           System.IO
import           Data.Time (getCurrentTime)
import           App

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    ts <- getCurrentTime
    loop (emptyState ts)

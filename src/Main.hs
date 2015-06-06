module Main where

import           System.IO
import           App

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    process []

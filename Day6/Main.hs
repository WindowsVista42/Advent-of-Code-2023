module Main where

import qualified Util

main :: IO ()
main = do
    lines' <- Util.readFile "Day6/input.txt"
    print lines'
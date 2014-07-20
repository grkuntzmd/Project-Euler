module Main where

import Data.List

main :: IO ()
main = do
    print $ head $ drop 999999 $ sort $ permutations "0123456789"

module Main where

import Data.Char (digitToInt)

main :: IO ()
main = print $ sum $ map digitToInt $ show value
  where
    value :: Integer
    value = 2 ^ (1000 :: Integer)
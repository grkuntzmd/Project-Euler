module Main where

import Data.Char (digitToInt)

main :: IO ()
main =
    print $ sum $ map digitToInt $ show $ factorial 100
  where
    factorial :: Integer -> Integer
    factorial n = product [1..n]
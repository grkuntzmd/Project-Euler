module Main where

import Data.Function (on)
import Data.List (maximumBy)

main :: IO ()
main = print runCollatz

runCollatz :: Integer
runCollatz =
    head $
    maximumBy (compare `on` length) $
    map (\n -> collatz [n]) [1..1000000]

collatz :: [Integer] -> [Integer]
collatz []                = []
collatz (x:_)   | x <= 0  = [0]
collatz s@(x:_) | x == 1  = reverse s
collatz s@(x:_)
    | even x              = collatz ((x `div` 2) : s)
    | otherwise           = collatz ((x * 3 + 1) : s)
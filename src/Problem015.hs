module Main where

main :: IO ()
main = print $ permutations 20

permutations :: Integer -> Integer
permutations n = product [(n + 1) .. (n * 2)] `div` product [2..n]
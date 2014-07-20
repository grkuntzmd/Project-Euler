module Main where

main :: IO ()
main = do
  let result = sum $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [1..999]
  print result
module Main where

main :: IO ()
main = do
  --let result = head $ filter (\n -> all (\d -> n `mod` d == 0) [2..20]) [20..]
  let result = foldr1 lcm [1..20]
  print result
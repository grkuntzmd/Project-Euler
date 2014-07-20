module Main where

import qualified Data.Map as Map

import Sieve (primes)

main :: IO ()
main = do
  let highest = last $ factors 600851475143
  print highest

-- Calculate all prime factors of a number
factors :: Int -> [Int]
factors n = loop n primes []
  where
    loop :: Int -> [Int] -> [Int] -> [Int]
    loop 1 _      acc = reverse acc
    loop n primes' acc =
      let prime = head primes'
      in
        if n `mod` prime == 0
          then loop (n `div` prime) primes' (prime : acc)
          else loop n (tail primes') acc

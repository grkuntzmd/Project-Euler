module Main where

import Sieve (primes)

main :: IO ()
main = (print . sum . takeWhile (<2000000)) primes
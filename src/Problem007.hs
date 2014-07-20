module Main where

import qualified Data.Map as Map

import Sieve (primes)

main :: IO ()
main = print $ primes !! 10000

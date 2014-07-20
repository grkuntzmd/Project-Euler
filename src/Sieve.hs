module Sieve
  ( primes
  ) where

import qualified Data.Map as Map

primes :: [Int]
primes = sieve [2..]

-- From http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
-- Calculate an infinite sequence of prime numbers
sieve :: [Int] -> [Int]
sieve xs = sieve' xs Map.empty
  where
    sieve' []       table = []
    sieve' (x : xs) table =
        case Map.lookup x table of
          Nothing    -> x : sieve' xs (Map.insert (x * x) [x] table)
          Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
      where
        reinsert table prime = Map.insertWith (++) (x + prime) [prime] table

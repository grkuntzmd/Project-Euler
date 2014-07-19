module Main where

import           Data.Function       (on)
import           Data.List           (maximumBy)
import           Data.Numbers.Primes (isPrime)

main :: IO ()
main = print $ maximumBy (compare `on` snd) [ (a * b, conseqPrimes a b) | b <- [(-99)..99], isPrime (abs b), a <- [(-99)..99]]

conseqPrimes :: Int -> Int -> Int
conseqPrimes a b = length $ takeWhile isPrime $ map (\n -> n * n + a * n + b) [0..]

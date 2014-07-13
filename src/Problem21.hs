module Main where

import Data.List (nub, sort)
import Debug.Trace
debug :: Show a => String -> a -> a
debug description value =
    trace (description ++ ": " ++ show value) value

main :: IO ()
main =
    print $
    sum $
    filter other [2..10000]
  where
    other n =
        let f = factors n
            s = sum f
            f' = factors s
            s' = sum f'
        in  s' == n && s /= n

factors :: Int -> [Int]
factors n =
    init $
    nub $
    sort $
    concatMap (\f -> [f, n `div` f]) $
    filter (\c -> n `mod` c == 0) [1..limit n]
  where
    limit :: Int -> Int
    limit = ceiling . sqrt . fromIntegral

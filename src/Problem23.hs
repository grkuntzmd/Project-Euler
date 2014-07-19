module Main where

import Data.List (nub, sort)
import Data.Set ((\\), fromList, toList)

main :: IO ()
main =
    print $
    sum $
    toList $
    fromList [1..limit] \\ fromList (abundantSums limit)

abundantSums :: Int -> [Int]
abundantSums lim =
    loop [[]] $ abundants lim
  where
    loop :: [[Int]] -> [Int] -> [Int]
    loop acc [] = (nub . sort . concat) acc
    loop acc c@(x:xs) = loop (map (+ x) c : acc) xs

limit :: Int
limit = 28123

abundants :: Int -> [Int]
abundants lim =
    map fst $
    filter (\(n, s) ->
        s > n) $
    map (\n ->
        (n, sum $ factors n)) [2..lim]

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

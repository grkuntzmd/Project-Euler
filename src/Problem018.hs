{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import Debug.Trace
debug :: Show a => String -> a -> a
debug description value =
    trace(description ++ ": " ++ show value) value

main :: IO ()
main = do
    let rev = reverse triangle
    print $ head $ loop rev
  where
    loop :: [[Int]] -> [Int]
    loop [current] = current
    loop (below:current:rest) =
        let row = loopRow below current
        in  loop (row:rest)

    loopRow :: [Int] -> [Int] -> [Int]
    loopRow _ [] = []
    loopRow (belowLeft:belowRight:belows) (current:currents) =
        let left = current + belowLeft
            right = current + belowRight
        in  left `max` right : loopRow (belowRight:belows) currents

triangle :: [[Int]]
triangle =
    [ [ 75 ]
    , [ 95, 64 ]
    , [ 17, 47, 82 ]
    , [ 18, 35, 87, 10 ]
    , [ 20,  4, 82, 47, 65 ]
    , [ 19,  1, 23, 75,  3, 34 ]
    , [ 88,  2, 77, 73,  7, 63, 67 ]
    , [ 99, 65,  4, 28,  6, 16, 70, 92 ]
    , [ 41, 41, 26, 56, 83, 40, 80, 70, 33 ]
    , [ 41, 48, 72, 33, 47, 32, 37, 16, 94, 29 ]
    , [ 53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14 ]
    , [ 70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57 ]
    , [ 91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48 ]
    , [ 63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31 ]
    , [  4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23 ]
    ]

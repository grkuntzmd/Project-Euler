module Main where

main :: IO ()
main = print find501

find501 :: Int
find501 =
    head $
    filter (\n ->
        let f = factors n
            l = length f
        in  l > 500) triangle
  where
    factors :: Int -> [Int]
    factors n =
        concatMap (\f -> [f, n `div` f]) $
        filter (\c -> n `mod` c == 0) [1..limit n]
      where
        limit :: Int -> Int
        limit = ceiling . sqrt . fromIntegral

    triangle :: [Int]
    triangle = scanl1 (+) [1..]
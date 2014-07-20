module Main where

main :: IO ()
main = do
    let sumOfSquares = sum $ map sqr [1..100]
        squareOfSums = sqr $ sum [1..100]
    print (squareOfSums - sumOfSquares)
  where
    sqr n = n * n
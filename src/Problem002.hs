module Main where

main :: IO ()
main = do
    let result = sum $ filter even $ takeWhile (<= 4000000) fibs
    print result
  where
    fibs = 0 : loop 0 1

    loop a b = b : loop b (a + b)
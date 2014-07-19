module Main where

main :: IO ()
main = print $ fst $ head $ dropWhile (\(n, f) ->
        length (show f) < 1000) $ zip [1..] fibs
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

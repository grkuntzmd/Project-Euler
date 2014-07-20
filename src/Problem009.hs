module Main where

main :: IO ()
main =
    print (head [x * y * hyp
                | x <- [1..1000]
                , y <- [x..1000]
                , let h = sqrt $ fromInteger (x ^ 2 + y ^ 2)
                , isInt h
                , let hyp = floor h
                , x + y + hyp == 1000])
  where
    isInt :: Float -> Bool
    isInt x = x == fromInteger (floor x)

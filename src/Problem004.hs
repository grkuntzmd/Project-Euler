module Main where

main :: IO ()
main = do
  let result =
        maximum [res | x <- [100..999]
                     , y <- [x..999]
                     , let res = x * y
                     , isPalindrome res]
  print result

isPalindrome :: Int -> Bool
isPalindrome n =
  let str = show n
  in
    str == reverse str

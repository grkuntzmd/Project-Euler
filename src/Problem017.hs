module Main where

import Data.Array ((!), listArray)
import Data.Char (isLetter)
import Data.List (intercalate)

main :: IO ()
main = print $
       length $
       filter isLetter $
       intercalate "" $
       map wordize [1..1000]

wordize :: Int -> String
wordize 1000 = "one thousand"
wordize n =
    let hundreds = n `div` 100
        tensUnits = n `mod` 100
        tens = tensUnits `div` 10
        units = tensUnits `mod` 10

        hundredsText =
            if hundreds == 0
                then ""
                else
                    smallNumberWords ! hundreds ++ " hundred" ++
                    if tensUnits == 0 then "" else " and "

        tensText =
            case tens of
                _ | tens >= 2 ->
                    tenWords ! tens ++
                    if units /= 0
                        then "-" ++ smallNumberWords ! units
                        else ""
                _ | tensUnits /= 0 -> smallNumberWords ! tensUnits
                _ -> ""
    in
        hundredsText ++ tensText
  where
    smallNumberWords = listArray (0 :: Int, 19)
      [ "zero"
      , "one"
      , "two"
      , "three"
      , "four"
      , "five"
      , "six"
      , "seven"
      , "eight"
      , "nine"
      , "ten"
      , "eleven"
      , "twelve"
      , "thirteen"
      , "fourteen"
      , "fifteen"
      , "sixteen"
      , "seventeen"
      , "eighteen"
      , "nineteen"
      ]
    tenWords = listArray (0 :: Int, 9)
      [ ""
      , ""
      , "twenty"
      , "thirty"
      , "forty"
      , "fifty"
      , "sixty"
      , "seventy"
      , "eighty"
      , "ninety"
      ]

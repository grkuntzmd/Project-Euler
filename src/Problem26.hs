module Main where

import           Data.Function (on)
import           Data.List     (maximumBy)
import qualified Data.Map      as M

main :: IO ()
main =
    print $
    fst $
    maximumBy (compare `on` snd) $
    map (\d -> (d, repetitionSize d)) [2..1000]

repetitionSize :: Int -> Int
repetitionSize den =
    repetitionSize' 0 1 M.empty
    where
          repetitionSize' :: Int -> Int -> M.Map Int Int -> Int
          repetitionSize' index num seen =
              let remainder = num `mod` den
              in  if remainder == 0
                  then 0
                  else
                    case M.lookup remainder seen of
                            Just i -> index - i
                            Nothing -> repetitionSize' (index + 1) (remainder * 10) (M.insert remainder index seen)

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.List (sort)
import Data.List.Split (splitOn)
import Text.Regex (mkRegex, subRegex)

main :: IO ()
main = do
    namesFile <- readFile "names.txt"
    print $
        sum $
        map (\(index, name) -> sum (map (\c -> ord c - ord 'A' + 1) name) * index) $
        zip [1..] $
        sort $
        map removeQuotes $
        splitOn "," namesFile

  where
    regex = mkRegex "^\"(.*)\"$"

    removeQuotes s = subRegex regex s "\\1"
module Main where

data Date = Date Int Int Int deriving (Eq, Show)

main :: IO ()
main =
    print $
    length $
    filter (\(dayOfWeek, Date _ _ day) -> dayOfWeek == 6 && day == 1) $
    takeWhile (\(day, date) -> date /= Date 2000 12 31) $
    dropWhile (\(day, date) -> date /= Date 1901 1 1) $
    zip (cycle [0..6]) (nextDates (Date 1900 1 1))

nextDates :: Date -> [Date]
nextDates d =
    let next = nextDate d
    in  d : nextDates next
  where
    nextDate :: Date -> Date
    nextDate (Date year month day) =
        let lastOfMonth =
                case month of
                    1 -> 31
                    2   | year `mod` 400 == 0 -> 29
                        | year `mod` 100 == 0 -> 28
                        | year `mod` 4 == 0   -> 29
                        | otherwise           -> 28
                    3 -> 31
                    4 -> 30
                    5 -> 31
                    6 -> 30
                    7 -> 31
                    8 -> 31
                    9 -> 30
                    10 -> 31
                    11 -> 30
                    12 -> 31
            advanceMonth = day == lastOfMonth
            advanceYear = advanceMonth && month == 12
            day' = if day == lastOfMonth then 1 else day + 1
            month' =
                if advanceMonth
                    then  if month == 12 then 1 else month + 1
                    else month
            year' = if advanceYear then year + 1 else year
        in  Date year' month' day'

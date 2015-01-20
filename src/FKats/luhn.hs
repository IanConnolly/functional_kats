module FKats.Luhn (luhn) where

import Data.Char (digitToInt)

luhn :: String -> Bool
luhn s = let numbers = numberfied s
         in lastDigit (partial1 numbers + partial2 numbers) == 0
         where lastDigit x = x `mod` 10
               numberfied = map digitToInt . reverse
               partial1 = sum . takeNthFromStart 2
               partial2 = sum . map (sumDigits . (*2)) . everyNth 2
               sumDigits = sum . map digitToInt . show

takeNthFromStart :: Int -> [a] -> [a]
takeNthFromStart n (x:xs) = x : everyNth n xs

everyNth :: Int -> [a] -> [a]
everyNth n xs = case drop (n - 1) xs of
                    (y:ys) -> y : everyNth n ys
                    [] -> []
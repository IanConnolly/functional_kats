module Main where

import System.Environment
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


main :: IO ()
main = do
    args <- getArgs
    if length args == 0 then
        error "'Gis an aul argument there, please."
    else
        case head args of
            "luhn" -> do
                let test = ["49927398716", "49927398717", "1234567812345678", "1234567812345670"]
                let results = map luhn test
                putStrLn $ "Results: " ++ show (zip test results)
            _ -> error "¯\\_(ツ)_/¯"
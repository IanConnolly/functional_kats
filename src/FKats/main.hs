module Main where

import FKats.Luhn
import System.Environment

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
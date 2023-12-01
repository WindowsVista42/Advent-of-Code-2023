module Main where

import Data.Char
import Data.Maybe
import Data.List

-- Part 1

part1 :: IO ()
part1 = do
    contents <- readFile "Day1/input.txt"
    let lines' = lines contents
    let firstDigits = map (\x -> fromJust $ find isDigit x) lines'
    let lastDigits = map (\x -> fromJust $ find isDigit $ reverse x) lines'
    let numbers = zipWith (\a b -> read [a, b] :: Int) firstDigits lastDigits
    let sum' = sum numbers
    print sum'

-- Part 2

numberMap :: [(String, Char)]
numberMap =
    [ ("one",   '1')
    , ("two",   '2')
    , ("three", '3')
    , ("four",  '4')
    , ("five",  '5')
    , ("six",   '6')
    , ("seven", '7')
    , ("eight", '8')
    , ("nine",  '9')
    , ("1", '1')
    , ("2", '2')
    , ("3", '3')
    , ("4", '4')
    , ("5", '5')
    , ("6", '6')
    , ("7", '7')
    , ("8", '8')
    , ("9", '9')
    ]

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString searchTerm str = findIndex (isPrefixOf searchTerm) (tails str)

getFirstNum :: String -> [(String, Char)] -> (String -> String) -> Char
getFirstNum xs searchTerms fn =
    let (_, v) = foldl (\acc (x, ch) -> getClosest (findString (fn x) (fn xs), ch) acc) (length xs, Nothing) searchTerms
    in fromJust v
    where
        getClosest :: (Maybe Int, Char) -> (Int, Maybe Char) -> (Int, Maybe Char)
        getClosest (foundIdx, foundAsChar) (accIdx, accAsChar)
            | fromMaybe (maxBound :: Int) foundIdx < accIdx = (fromJust foundIdx, Just foundAsChar)
            | otherwise = (accIdx, accAsChar)

part2 :: IO ()
part2 = do
    contents <- readFile "Day1/input.txt"
    let lines' = lines contents
    let firstDigits = map (\x -> getFirstNum x numberMap id) lines'
    let lastDigits = map (\x -> getFirstNum x numberMap reverse) lines'
    let numbers = zipWith (\a b -> read [a,b] :: Int) firstDigits lastDigits
    let sum' = sum numbers
    print sum'

main :: IO ()
main = do
    part1
    part2
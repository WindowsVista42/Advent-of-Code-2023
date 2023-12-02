module Main where

import Data.Maybe

-- Part 1

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
  (ls, "") -> [ls]
  (ls, _:rs) -> ls : split c rs

cleanupOutput :: String -> String
cleanupOutput xs = drop 5 $ filter (\x -> x/=':' && x/=';' && x/=',') xs

splitOutput :: String -> [String]
splitOutput = split ' '

isValid :: [String] -> Bool
isValid [] = True
isValid [_] = True
isValid (x:y:xs) =
    let n = read x :: Int
    in
        if y == "red" then
            n <= 12 && isValid xs
        else
            if y == "green" then
                n <= 13 && isValid xs
            else
                if y == "blue" then
                    n <= 14 && isValid xs
                else
                    isValid xs

isPossible :: String -> Maybe Int
isPossible line =
    let xs = splitOutput $ cleanupOutput line
        num = read $ head xs :: Int
    in
        if isValid (tail xs) then
            Just num
        else
            Nothing

part1 :: IO ()
part1 = do
    contents <- readFile "Day2/input.txt"
    let lines' = lines contents
    let possible = mapMaybe isPossible lines'
    let sum' = sum possible
    print sum'

-- Part 2

doTheThing :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
doTheThing acc [] = acc
doTheThing acc [_] = acc
doTheThing (r, g, b) (x:y:xs) =
    let n = read x :: Int
    in
        if y == "red" then
            doTheThing (max r n , g, b) xs
        else
            if y == "green" then
                doTheThing (r, max g n, b) xs
            else
                if y == "blue" then
                    doTheThing (r, g, max b n) xs
                else
                    doTheThing (r, g, b) xs

calcPower :: String -> Int
calcPower line = 
    let (r,g,b) = doTheThing (0, 0, 0) $ tail $ splitOutput $ cleanupOutput line in r * g * b

part2 :: IO ()
part2 = do
    contents <- readFile "Day2/input.txt"
    let lines' = lines contents
    let powers = map calcPower lines'
    let sum' = sum powers
    print sum'

main :: IO ()
main = do
    part1
    part2
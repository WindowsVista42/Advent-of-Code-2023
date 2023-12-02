module Main where

import Data.Maybe

-- Shared

whenMaybe :: Bool -> a -> Maybe a
whenMaybe False _ = Nothing
whenMaybe True a = Just a

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
  (ls, [])   -> [ls]
  (ls, _:rs) -> ls : split c rs

cleanupOutput :: String -> String
cleanupOutput xs = drop 5 $ filter (\x -> x/=':' && x/=';' && x/=',') xs

splitOutput :: String -> [String]
splitOutput = split ' '

-- Part 1

isValid :: [String] -> Bool
isValid [] = True
isValid [_] = True
isValid (x:y:xs) =
    let n = read x :: Int
    in case y of
        "red"   -> n <= 12 && isValid xs
        "green" -> n <= 13 && isValid xs
        "blue"  -> n <= 14 && isValid xs
        _       -> isValid xs

isPossible :: String -> Maybe Int
isPossible line =
    let xs = splitOutput $ cleanupOutput line
        num = read $ head xs :: Int
    in 
        whenMaybe (isValid $ tail xs) num

part1 :: IO ()
part1 = do
    contents <- readFile "Day2/input.txt"
    let lines' = lines contents
    let possible = mapMaybe isPossible lines'
    let sum' = sum possible
    print sum'

-- Part 2

getTotalPower :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
getTotalPower acc [] = acc
getTotalPower acc [_] = acc
getTotalPower (r, g, b) (x:y:xs) =
    let n = read x :: Int
    in case y of
        "red"   -> getTotalPower (max r n, g, b) xs
        "green" -> getTotalPower (r, max g n, b) xs
        "blue"  -> getTotalPower (r, g, max b n) xs
        _       -> getTotalPower (r, g, b) xs

totalPower :: [String] -> (Int, Int, Int)
totalPower = getTotalPower (0, 0, 0)

calcPower :: String -> Int
calcPower line = 
    let (r,g,b) = totalPower $ tail $ splitOutput $ cleanupOutput line
    in
        r * g * b

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
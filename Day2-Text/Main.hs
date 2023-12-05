module Main where

import Data.Maybe
import Data.Either

import qualified Data.Text as T
import qualified Data.Text.Read as TR

-- Shared

whenMaybe :: Bool -> a -> Maybe a
whenMaybe False _ = Nothing
whenMaybe True a = Just a

cleanupOutput :: T.Text -> T.Text
cleanupOutput xs = T.drop 5 $ T.filter (\x -> x/=':' && x/=';' && x/=',') xs

splitOutput :: T.Text -> [T.Text]
splitOutput = T.splitOn $ T.pack " "

-- Part 1

isValid :: [T.Text] -> Bool
isValid [] = True
isValid [_] = True
isValid (x:y:xs) =
    let n = read $ T.unpack x :: Int
    in case T.unpack y of
        "red"   -> n <= 12 && isValid xs
        "green" -> n <= 13 && isValid xs
        "blue"  -> n <= 14 && isValid xs
        _       -> isValid xs

isPossible :: T.Text -> Maybe Int
isPossible line =
    let xs = splitOutput $ cleanupOutput line
        num = read $ T.unpack $ head xs :: Int
    in 
        whenMaybe (isValid $ tail xs) num

part1 :: IO ()
part1 = do
    contents <- readFile "Day2/input.txt"
    let lines' = map T.pack $ lines contents
    let possible = mapMaybe isPossible lines'
    let sum' = sum possible
    print sum'

-- Part 2

getHighestPower :: Int -> T.Text -> (Int,Int,Int) -> (Int,Int,Int)
getHighestPower n y (r,g,b)
    | y == T.pack "red"   = (max r n, g, b)
    | y == T.pack "green" = (r, max g n, b)
    | y == T.pack "blue"  = (r, g, max b n)
    | otherwise           = (r, g, b)

getTotalPower :: (Int, Int, Int) -> [T.Text] -> (Int, Int, Int)
getTotalPower acc [] = acc
getTotalPower acc [_] = acc
getTotalPower (r,g,b) (x:y:xs) =
    let (n,_) = fromRight (0, T.empty) $ TR.decimal x
    in
        getTotalPower (getHighestPower n y (r,g,b)) xs

totalPower :: [T.Text] -> (Int, Int, Int)
totalPower = getTotalPower (0, 0, 0)

calcPower :: T.Text -> Int
calcPower line = 
    let (r,g,b) = totalPower $ tail $ splitOutput $ cleanupOutput line
    in
        r * g * b

part2 :: IO ()
part2 = do
    contents <- readFile "Day2/input.txt"
    let lines' = map T.pack $ lines contents
    let powers = map calcPower lines'
    let sum' = sum powers
    print sum'

main :: IO ()
main = do
    part1
    part2
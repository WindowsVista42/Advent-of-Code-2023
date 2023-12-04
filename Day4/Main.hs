module Main where

-- This solution didn't give the correct answers for the sample text
-- but did give the correct answers for the input text
-- ... weird!

import Data.List
import Data.Maybe

import Data.Bifunctor

import qualified Data.Vector as V

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
  (ls, [])   -> [ls]
  (ls, _:rs) -> ls : split c rs

getNumOfMatches :: ([Int],[Int]) -> Int
getNumOfMatches (a,b) =
    length $ mapMaybe (\x -> find (== x) b) a

part1 :: [Int] -> Int
part1 = foldl (\acc x -> acc + if x == 0 then 0 else 2 ^ (x - 1)) 0

getUpdates :: Int -> (Int,Int) -> V.Vector Int -> V.Vector (Int, Int)
getUpdates c (lpos,rpos) v =
    (\x -> (x, (v V.! x) + c)) <$> V.fromList [lpos..rpos]

part2 :: [Int] -> Int
part2 matchCount =
    let acc = V.fromList $ replicate (length matchCount) 1
    in
        let it = V.foldl foldFn acc $ V.zip (V.fromList matchCount) (V.fromList [0..])
            foldFn :: V.Vector Int -> (Int, Int) -> V.Vector Int
            foldFn acc2 (m, i) =
                if m /= 0 then
                    let updates = getUpdates (acc2 V.! i) (i+1,i+m) acc2
                    in
                        V.update acc2 updates
                else
                    acc2
        in
            sum it

calcMatchCounts :: [String] -> [Int]
calcMatchCounts lines' =
    let lines'' = map (drop 10) lines'

        splitted = map (split '|') lines''
        eachNum = fmap fn splitted where
            fn [x,y] = (split ' ' x,split ' ' y)
            fn _ = undefined

        validNums = fmap (bimap fn fn) eachNum where
            fn = filter (/="")
        nums = fmap (bimap fn fn) validNums where
            fn = foldl (\acc x -> (read x :: Int) : acc) []
    in
        map getNumOfMatches nums

main :: IO ()
main = do
    contents <- readFile "Day4/input.txt"
    let lines' = lines contents
    let matchCount = calcMatchCounts lines'

    print $ part1 matchCount
    print $ part2 matchCount
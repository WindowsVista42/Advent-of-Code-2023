module Main where

import Data.Maybe
import Data.Char

import qualified Data.Vector as V
import qualified Text.Read as TR
import qualified Data.Map as M

isSymbolThatCounts :: Char -> Bool
isSymbolThatCounts x = x=='/' || x=='*' || x=='$' || x=='@' || x=='-' || x=='%' || x=='!' || x=='#' || x=='+' || x=='=' || x=='&' || x=='^'

anySymbolsInXY :: (Int, Int) -> (Int, Int) -> V.Vector (V.Vector Char) -> Bool
anySymbolsInXY (minx,miny) (maxx,maxy) v =
    let xs = [minx..maxx]
        ys = [miny..maxy]
    in
        foldl (\acc iy -> acc || foldl (\acc2 ix -> acc2 || isSymbolThatCounts (tryGet iy ix)) False xs) False ys where
            tryGet iy ix =
                let vy = fromMaybe (V.fromList []) (v V.!? iy)
                in fromMaybe '.' (vy V.!? ix)

anySymbolsForNumber :: (Int,Int) -> Int -> V.Vector (V.Vector Char) -> Bool
anySymbolsForNumber (minx,maxx) y v = 
    anySymbolsInXY (minx-1,y-1) (maxx+1,y+1) v

ballin :: V.Vector (V.Vector Char) -> V.Vector Char -> Int -> Int
ballin va v y =
    let z = V.groupBy (\(a,_) (b,_) -> isNumber a && isNumber b) $ V.zip v (V.fromList [0..])
        z' = map V.toList z
        z'' = filter f $ z' where
            f ((c,_):_) = c/='.'
            f [] = False
    in
        let b = map (\x -> foldr (\(c,_) acc -> c : acc) [] x) z''
            b2 = map (\x -> fromMaybe 0 (TR.readMaybe x :: Maybe Int)) b
            se = map (\x -> f2 x) z'' where
                f2 x = 
                    let (_,si) = head x
                        (_,ei) = last x
                    in
                        (si,ei)
        in
            foldr (\(n,s) acc -> if anySymbolsForNumber s y va then acc + n else acc) 0 $ zip b2 se

part1 :: V.Vector (V.Vector Char) -> Int
part1 va =
    V.foldr (\(v0,i) acc -> acc + ballin va v0 i) 0 $ V.zip va (V.fromList [0..])

main :: IO ()
main = do
    contents <- readFile "Day3/input.txt"
    let lines' = lines contents
    let a = V.fromList lines'
    let b = V.map V.fromList a
    print $ part1 b

    -- let v = b V.! 0

    -- let k = V.dropWhile (\(x,_) -> x=='.') $ V.zip v (V.fromList [0..])
    -- let l = V.takeWhile (\(x,_) -> isDigit x) k

    -- let n2 = V.foldr (\(v0,i) acc -> acc + ballin b v0 i) 0 $ V.zip b (V.fromList [0..])
    -- print n2

    -- let (_,firstIdx) = V.head l
    -- let (_,lastIdx) = V.last l
    -- let (_,r2) = V.splitAt (lastIdx+1) v

    -- let k2 = V.dropWhile (\(x,_) -> x=='.') $ V.zip r2 (V.fromList [0..])
    -- let l2 = V.takeWhile (\(x,_) -> isDigit x) k2
    -- let s2 = V.foldr (\(c,_) acc -> c : acc) [] l2
    -- print s2

    -- print (firstIdx,lastIdx)

    -- let c = anySymbolsForNumber (7,9) 2 b
    -- print c
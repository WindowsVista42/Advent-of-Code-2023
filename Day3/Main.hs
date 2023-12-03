module Main where

import Data.Maybe
import Data.Char
import Data.List

import qualified Data.Vector as V
import qualified Text.Read as TR
import qualified Data.Map as M

type ComponentList = ([Int], [(Int, Int)])

part1Symbol :: Char -> Bool
part1Symbol x = x=='/' || x=='*' || x=='$' || x=='@' || x=='-' || x=='%' || x=='!' || x=='#' || x=='+' || x=='=' || x=='&' || x=='^'

part2Symbol :: Char -> Bool
part2Symbol x = x == '*'

getSquareCoords :: (Int, Int) -> (Int, Int) -> ([Int],[Int])
getSquareCoords (minx,miny) (maxx,maxy) = ([minx..maxx], [miny..maxy])

anySymbolsInXY :: (Int, Int) -> (Int, Int) -> (Char -> Bool) -> V.Vector (V.Vector Char) -> Bool
anySymbolsInXY minpos maxpos fn va =
    let (xs,ys) = getSquareCoords minpos maxpos
    in
        foldl (\acc iy -> acc || foldl (\acc2 ix -> acc2 || fn (tryGet iy ix)) acc xs) False ys where
            tryGet iy ix =
                let vy = fromMaybe (V.fromList []) (va V.!? iy)
                in fromMaybe '.' (vy V.!? ix)

getSymbolPosInXY :: (Int, Int) -> (Int, Int) -> V.Vector (V.Vector Char) -> (Int, Int)
getSymbolPosInXY minpos maxpos va =
    let (xs,ys) = getSquareCoords minpos maxpos 
    in
        foldl (\acc iy -> foldl (\acc2 ix -> if part2Symbol $ tryGet iy ix then (ix,iy) else acc2) acc xs) (0,0) ys where
            tryGet iy ix =
                let vy = fromMaybe (V.fromList []) (va V.!? iy)
                in fromMaybe '.' (vy V.!? ix)

anySymbolsForNumber :: (Int,Int) -> Int -> (Char -> Bool) -> V.Vector (V.Vector Char) -> Bool
anySymbolsForNumber (minx,maxx) y fn va = anySymbolsInXY (minx-1,y-1) (maxx+1,y+1) fn va

getSymbolPosForNumber :: (Int, Int) -> Int -> V.Vector (V.Vector Char) -> (Int, Int)
getSymbolPosForNumber (minx,maxx) y va = getSymbolPosInXY (minx-1,y-1) (maxx+1,y+1) va

getComponentListForRow :: V.Vector Char -> ComponentList
getComponentListForRow v =
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
            (b2, se)

part1 :: V.Vector (V.Vector Char) -> Int
part1 va =
    V.foldr (\(v0,i) acc -> acc + f v0 i) 0 $ V.zip va (V.fromList [0..]) where
        f v0 i =
            let (nums, ranges) = getComponentListForRow v0
            in
                foldr (\(n,s) acc -> if anySymbolsForNumber s i part1Symbol va then acc + n else acc) 0 $ zip nums ranges

getPart2Ordering :: (Int, Int) -> (Int,Int) -> Ordering
getPart2Ordering (lx, ly) (rx, ry)
    | ly < ry = LT
    | ly > ry = GT
    | ly == ry && lx < rx = LT
    | ly == ry && lx > rx = GT
    | ly == ry && lx == rx = EQ
    | otherwise = EQ

part2 :: V.Vector (V.Vector Char) -> Int -- [V.Vector (Int, (Int, Int), Int, (Int, Int))]
part2 va =
    let a = V.foldl (\acc (v0,i) -> acc ++ f v0 i) [] $ V.zip va (V.fromList [0..]) where
        f v0 i =
            let (nums, ranges) = getComponentListForRow v0
            in
                let matches = foldl (\acc (n,s) -> if anySymbolsForNumber s i part2Symbol va then (n,s,i) : acc else acc) [] $ zip nums ranges
                in
                    foldl (\acc (num,pos,y) -> (num,pos,y,getSymbolPosForNumber pos y va) : acc) [] matches
    in
        let sorted = sortBy fn a where
            fn (_, _, _, lsympos) (_, _, _, rsympos) = getPart2Ordering lsympos rsympos
        in
            let filtered  = filter (\(n,_,_,sympos) -> if n == 0 || sympos == (0,0) then False else True) sorted
                grouped   = V.groupBy (\(_,_,_,lsympos) (_,_,_,rsympos) -> lsympos == rsympos) $ V.fromList filtered
                filtered2 = filter (\x -> length x > 1) grouped
                listified = map (\x -> V.toList x) filtered2
            in
                foldl (\acc [(ln,_,_,_),(rn,_,_,_)] -> acc + ln * rn) 0 listified

main :: IO ()
main = do
    contents <- readFile "Day3/input.txt"
    let lines' = lines contents
    let a = V.fromList lines'
    let b = V.map V.fromList a
    print $ part1 b
    print $ part2 b
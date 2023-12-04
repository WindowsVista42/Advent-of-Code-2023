module Main where

import Data.Maybe
import Data.Char
import Data.List

import qualified Data.Vector as V
import qualified Text.Read as TR

type ComponentList = ([Int], [(Int, Int)])
type ComponentInfo = (Int, Pos, Int, Pos)
type Pos = (Int, Int)
type IntRange = (Int, Int)
type Row = V.Vector Char
type Grid = V.Vector (V.Vector Char)

part1Symbol :: Char -> Bool
part1Symbol x = x=='/' || x=='*' || x=='$' || x=='@' || x=='-' || x=='%' || x=='!' || x=='#' || x=='+' || x=='=' || x=='&' || x=='^'

part2Symbol :: Char -> Bool
part2Symbol x = x == '*'

foldGrid :: Pos -> Pos -> (b -> Pos -> Char -> b) -> b -> Grid -> b
foldGrid (minX,minY) (maxX,maxY) foldFn initialValue grid =
    let (xs,ys) = ([minX..maxX], [minY..maxY])
    in
        foldl (\acc iy -> foldl (\acc2 ix -> foldFn acc2 (ix,iy) (tryGet iy ix) ) acc xs) initialValue ys where
            tryGet iy ix =
                let vy = fromMaybe (V.fromList []) (grid V.!? iy)
                in fromMaybe '.' (vy V.!? ix)

hasSymbolInXY :: Pos -> Pos -> (Char -> Bool) -> Grid -> Bool
hasSymbolInXY minPos maxPos fn =
    foldGrid minPos maxPos (\acc _ x -> acc || fn x) False

getSymbolPosInXY :: Pos -> Pos -> (Char -> Bool) -> Grid -> Pos
getSymbolPosInXY minPos maxPos fn =
    foldGrid minPos maxPos (\acc (ix,iy) x -> if fn x then (ix,iy) else acc) (0,0)

hasSymbolForNumber :: IntRange -> Int -> (Char -> Bool) -> Grid -> Bool
hasSymbolForNumber (minX,maxX) y =
    hasSymbolInXY (minX-1,y-1) (maxX+1,y+1)

getSymbolPosForNumber :: IntRange -> Int -> (Char -> Bool) -> Grid -> Pos
getSymbolPosForNumber (minX,maxX) y =
    getSymbolPosInXY (minX-1,y-1) (maxX+1,y+1)

toComponentList :: Row -> ComponentList
toComponentList row =
    let charAndPos = fmap V.toList $ V.groupBy (\(a,_) (b,_) -> isNumber a && isNumber b) $ V.zip row (V.fromList [0..])
        onlyNumsAndPos = filter filterFn charAndPos where
            filterFn ((c,_):_) = c/='.'
            filterFn [] = False
    in
        let numStrs = map (foldr (\(c,_) acc -> c : acc) []) onlyNumsAndPos
            nums    = map (\x -> fromMaybe 0 (TR.readMaybe x :: Maybe Int)) numStrs
            ranges  = map (\x -> (snd $ head x, snd $ last x)) onlyNumsAndPos
    in
        (nums, ranges)

getAllComponents :: Grid -> (Char -> Bool) -> [ComponentInfo]
getAllComponents grid fn =
    foldl foldFn [] $ V.zip grid (V.fromList [0..]) where
        foldFn foldAcc (row, y) =
            let (nums, ranges) = toComponentList row
                matches       = foldl (\acc (num, posRange) -> [(num, posRange, y) | hasSymbolForNumber posRange y fn grid] ++ acc) [] $ zip nums ranges
                componentInfo = foldl (\acc (num, pos, _) -> (num,pos,y,getSymbolPosForNumber pos y fn grid) : acc) [] matches
            in 
                foldAcc ++ componentInfo

part1 :: Grid -> Int
part1 grid =
    let allComponents = getAllComponents grid part1Symbol
    in
        foldl (\acc (num,_,_,_) -> acc + num) 0 allComponents

part2 :: Grid -> Int
part2 grid =
    -- The general algorithm for part 2 is the following steps:
    -- 1. Generate a list of components with a '*' symbol next to them
    -- 2. Find the position of the symbol for the components
    -- 3. Sort all components by the position of the symbol
    -- 4. Group components together that have the same symbol position
    -- 5. Remove groups with one or fewer elements
    -- 6. Multiply numbers in groups together

    let allComponents = getAllComponents grid part2Symbol
        sorted = sortBy fn allComponents where
            fn (_,_,_,lsympos) (_,_,_,rsympos) = compareFn lsympos rsympos where
                compareFn :: Pos -> Pos -> Ordering
                compareFn (lx, ly) (rx, ry)
                    | ly < ry = LT
                    | ly > ry = GT
                    | ly == ry && lx < rx = LT
                    | ly == ry && lx > rx = GT
                    | ly == ry && lx == rx = EQ
                    | otherwise = EQ
        filtered = filter (\(n,_,_,_) -> n /= 0) sorted
        grouped  = groupBy (\(_,_,_,lsympos) (_,_,_,rsympos) -> lsympos == rsympos) filtered
        valid    = filter (\x -> length x > 1) grouped
    in
        foldl (\acc x -> acc + foldFn x) 0 valid where
            foldFn [(ln,_,_,_),(rn,_,_,_)] = ln * rn
            foldFn _ = 0

main :: IO ()
main = do
    contents <- readFile "Day3/input.txt"
    let lines' = lines contents
    let grid = V.fromList <$> V.fromList lines'
    print $ part1 grid
    print $ part2 grid
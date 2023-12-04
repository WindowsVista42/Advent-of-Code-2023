module Main where

import Data.Maybe
import Data.Char
import Data.List

import qualified Data.Vector as V
import qualified Text.Read as TR

type ComponentList = ([Int], [(Int, Int)])
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
    let z = V.groupBy (\(a,_) (b,_) -> isNumber a && isNumber b) $ V.zip row (V.fromList [0..])
        z' = map V.toList z
        z'' = filter f z' where
            f ((c,_):_) = c/='.'
            f [] = False
    in
        let b = map (foldr (\(c,_) acc -> c : acc) []) z''
            b2 = map (\x -> fromMaybe 0 (TR.readMaybe x :: Maybe Int)) b
            se = map f2 z'' where
                f2 x = 
                    let (_,si) = head x
                        (_,ei) = last x
                    in
                        (si,ei)
        in
            (b2, se)

part1 :: Grid -> Int
part1 grid =
    V.foldl (\acc (row, y) -> acc + mapFn row y) 0 $ V.zip grid (V.fromList [0..]) where
        mapFn row y =
            let (nums, ranges) = toComponentList row
            in
                foldr (\(num, s) acc -> if hasSymbolForNumber s y part1Symbol grid then acc + num else acc) 0 $ zip nums ranges

part2 :: Grid -> Int
part2 grid =
    -- The general algorithm for part 2 is the following steps:
    -- 1. Generate a list of components with a '*' symbol next to them
    -- 2. Find the position of the symbol for the components
    -- 3. Sort all components by the position of the symbol
    -- 4. Group components together that have the same symbol position
    -- 5. Remove groups with one or fewer elements
    -- 6. Multiply numbers in groups together

    let allComponents = foldl (\acc (row, y) -> acc ++ mapFn row y) [] $ V.zip grid (V.fromList [0..])
        mapFn row y =
            let (nums, ranges) = toComponentList row
                matches = foldl (\acc (num, posRange) -> if hasSymbolForNumber posRange y part2Symbol grid then (num, posRange, y) : acc else acc) [] $ zip nums ranges
            in
                foldl (\acc (num, pos, _) -> (num,pos,y,getSymbolPosForNumber pos y part2Symbol grid) : acc) [] matches

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
        foldl (\acc x -> acc + fn2 x) 0 valid where
            fn2 [(ln,_,_,_),(rn,_,_,_)] = ln * rn
            fn2 _ = 0

main :: IO ()
main = do
    contents <- readFile "Day3/input.txt"
    let lines' = lines contents
    let a = V.fromList lines'
    let b = V.map V.fromList a
    print $ part1 b
    print $ part2 b
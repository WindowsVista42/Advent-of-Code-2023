module Main where

import Data.Either

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Read as T.Read

data InfoMapping = InfoMapping {
    dstMapping :: Int,
    srcMapping :: Int,
    range      :: Int
} deriving Show

type SeedId = Int

data SeedMaps = SeedMaps {
    seeds                 :: V.Vector SeedId,
    seedToSoil            :: V.Vector InfoMapping,
    soilToFertilizer      :: V.Vector InfoMapping,
    fertilizerToWater     :: V.Vector InfoMapping,
    waterToLight          :: V.Vector InfoMapping,
    lightToTemperature    :: V.Vector InfoMapping,
    temperatureToHumidity :: V.Vector InfoMapping,
    humidityToLocation    :: V.Vector InfoMapping
} deriving Show

parseInt :: T.Text -> Int
parseInt x = 
    fst $ fromRight (0, T.empty) $ T.Read.decimal x

findInfoMapping :: V.Vector InfoMapping -> Int -> Int
findInfoMapping m srcVal =
    foldl foldFn srcVal m where
        foldFn acc srcFoldVal =
            let InfoMapping dstMap srcMap range' = srcFoldVal
            in
                if srcVal >= srcMap && srcVal < (srcMap + range') then
                    dstMap + (srcVal - srcMap)
                else
                    acc

parseSeeds :: [T.Text] -> (V.Vector SeedId, [T.Text])
parseSeeds (x:xs) =
    let line    = T.drop 7 x
        numStrs = T.splitOn (T.pack " ") line
        nums    = V.fromList $ fmap parseInt numStrs
    in
        (nums, dropWhile (==T.pack "\n") xs)

parseSeeds _ = undefined

infoMappingFromText :: [T.Text] -> InfoMapping
infoMappingFromText [dstMapping',srcMapping',range'] = InfoMapping (parseInt dstMapping') (parseInt srcMapping') (parseInt range')
infoMappingFromText _ = undefined

skipLine :: [T.Text] -> [T.Text]
skipLine [] = []
skipLine (_:xs) = xs

skipEmptyLines :: [T.Text] -> [T.Text]
skipEmptyLines = dropWhile (==T.empty)

spanNonEmptyLines :: [T.Text] -> ([T.Text], [T.Text])
spanNonEmptyLines = span (/=T.empty)

parseInfoMapping :: [T.Text] -> (V.Vector InfoMapping, [T.Text])
parseInfoMapping xs =
    let (lhs,rhs) = spanNonEmptyLines $ skipLine $ skipEmptyLines xs
    in
        let lhs' = fmap (T.splitOn (T.pack " ")) lhs
        in
            let infoMappings = fmap infoMappingFromText lhs'
            in
                (V.fromList infoMappings, rhs)

getPairs :: [Int] -> [(Int,Int)]
getPairs [] = []
getPairs [_] = []
getPairs (x:y:xs) = (x,y) : getPairs xs

parseInput :: [T.Text] -> SeedMaps
parseInput lines' =
    let (seeds',l0)                 = parseSeeds lines'
        (seedToSoil',l1)            = parseInfoMapping l0
        (soilToFertilizer',l2)      = parseInfoMapping l1
        (fertilizerToWater',l3)     = parseInfoMapping l2
        (waterToLight',l4)          = parseInfoMapping l3
        (lightToTemperature',l5)    = parseInfoMapping l4
        (temperatureToHumidity',l6) = parseInfoMapping l5
        (humidityToLocation',_)     = parseInfoMapping l6
    in
        SeedMaps
            seeds'
            seedToSoil'
            soilToFertilizer'
            fertilizerToWater'
            waterToLight'
            lightToTemperature'
            temperatureToHumidity'
            humidityToLocation'

getMinLocation :: SeedMaps -> Int -> Int -> Int
getMinLocation seedMap acc x =
    let SeedMaps _ m0 m1 m2 m3 m4 m5 m6 = seedMap
        sv0 = findInfoMapping m0 x
        sv1 = findInfoMapping m1 sv0
        sv2 = findInfoMapping m2 sv1
        sv3 = findInfoMapping m3 sv2
        sv4 = findInfoMapping m4 sv3
        sv5 = findInfoMapping m5 sv4
        sv6 = findInfoMapping m6 sv5
    in
        min sv6 acc

part1 :: SeedMaps -> Int
part1 seedMaps =
    foldl (getMinLocation seedMaps) maxBound (seeds seedMaps)

part2 :: SeedMaps -> Int
part2 seedMaps =
    let seedPairs = V.fromList $ getPairs $ V.toList $ seeds seedMaps
    in
        V.foldl' foldFn maxBound seedPairs where
            foldFn acc (start', range') =
                V.foldl' (getMinLocation seedMaps) acc (V.fromList [start'..start'+range'])

main :: IO ()
main = do
    contents <- readFile "Day5/input.txt"
    let lines' = T.pack <$> lines contents
    let seedMaps = parseInput lines'

    print $ part1 seedMaps
    print $ part2 seedMaps
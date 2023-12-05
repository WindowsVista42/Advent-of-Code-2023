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
            let dstMap = dstMapping srcFoldVal
                srcMap = srcMapping srcFoldVal
                range' = range srcFoldVal
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

part1 :: SeedMaps -> Int
part1 m =
    let seeds' = seeds m
        s0 = seedToSoil m
        s1 = soilToFertilizer m
        s2 = fertilizerToWater m
        s3 = waterToLight m
        s4 = lightToTemperature m
        s5 = temperatureToHumidity m
        s6 = humidityToLocation m
    in
        foldl (foldFn (s0,s1,s2,s3,s4,s5,s6)) maxBound seeds' where
            foldFn (s0,s1,s2,s3,s4,s5,s6) acc x =
                let sv0 = findInfoMapping s0 x
                    sv1 = findInfoMapping s1 sv0
                    sv2 = findInfoMapping s2 sv1
                    sv3 = findInfoMapping s3 sv2
                    sv4 = findInfoMapping s4 sv3
                    sv5 = findInfoMapping s5 sv4
                    sv6 = findInfoMapping s6 sv5
                in
                    min sv6 acc

getPairs :: [Int] -> [(Int,Int)]
getPairs [] = []
getPairs [_] = []
getPairs (x:y:xs) = (x,y) : getPairs xs

part2 :: SeedMaps -> Int
part2 m =
    let seeds' = seeds m
        s0 = seedToSoil m
        s1 = soilToFertilizer m
        s2 = fertilizerToWater m
        s3 = waterToLight m
        s4 = lightToTemperature m
        s5 = temperatureToHumidity m
        s6 = humidityToLocation m
    in
        let seedPairs = V.fromList $ getPairs $ V.toList seeds'
        in
            V.foldl' (foldFn (s0,s1,s2,s3,s4,s5,s6)) maxBound seedPairs where
                foldFn (s0,s1,s2,s3,s4,s5,s6) acc (start', range') =
                    V.foldl' (foldFn' (s0,s1,s2,s3,s4,s5,s6)) acc (V.fromList [start'..start'+range']) where
                        foldFn' (s0',s1',s2',s3',s4',s5',s6') acc' x =
                            let sv0 = findInfoMapping s0' x
                                sv1 = findInfoMapping s1' sv0
                                sv2 = findInfoMapping s2' sv1
                                sv3 = findInfoMapping s3' sv2
                                sv4 = findInfoMapping s4' sv3
                                sv5 = findInfoMapping s5' sv4
                                sv6 = findInfoMapping s6' sv5
                            in
                                min sv6 acc'

main :: IO ()
main = do
    contents <- readFile "Day5/input.txt"
    let lines' = T.pack <$> lines contents
    let seedMaps = parseInput lines'

    print $ part1 seedMaps
    print $ part2 seedMaps
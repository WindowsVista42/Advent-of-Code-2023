import Data.Char
import Data.Maybe
import Data.List

part1 :: IO ()
part1 = do
    contents <- readFile "input.txt"
    let lines' = lines contents
    let firstDigits = map (\x -> fromJust $ find isDigit x) lines'
    let lastDigits = map (\x -> fromJust $ find isDigit $ reverse x) lines'
    let numbers = zipWith (\a b -> read [a, b] :: Int) firstDigits lastDigits
    let sum' = sum numbers
    print sum'

numberMap :: [(String, Char)]
numberMap =
    [ ("one",   '1')
    , ("two",   '2')
    , ("three", '3')
    , ("four",  '4')
    , ("five",  '5')
    , ("six",   '6')
    , ("seven", '7')
    , ("eight", '8')
    , ("nine",  '9')
    , ("1", '1')
    , ("2", '2')
    , ("3", '3')
    , ("4", '4')
    , ("5", '5')
    , ("6", '6')
    , ("7", '7')
    , ("8", '8')
    , ("9", '9')
    ]

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString token str = findIndex (isPrefixOf token) (tails str)

getFirstNum :: String -> [(String, Char)] -> (String -> String) -> Char
getFirstNum xs tokens fn =
    let (i, v) = foldl (\acc (x, c) -> getClosest (findString (fn x) (fn xs), c) acc) (length xs, Nothing) tokens
    in fromMaybe '0' v
    where
        getClosest :: (Maybe Int, Char) -> (Int, Maybe Char) -> (Int, Maybe Char)
        getClosest (foundIdx, foundNumAsChar) (accI, accV)
            | fromMaybe (maxBound :: Int) foundIdx < accI = (fromJust foundIdx, Just foundNumAsChar)
            | otherwise = (accI, accV)

part2 :: IO ()
part2 = do
    contents <- readFile "input.txt"
    let lines' = lines contents
    let firstDigits = map (\x -> getFirstNum x numberMap id) lines'
    let lastDigits = map (\x -> getFirstNum x numberMap reverse) lines'
    let numbers = zipWith (\a b -> read [a,b] :: Int) firstDigits lastDigits
    let sum' = sum numbers
    print sum'

main :: IO ()
main = do
    part1
    part2
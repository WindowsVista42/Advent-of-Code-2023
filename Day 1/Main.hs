import Data.Char
import Data.Maybe
import Data.List

part1 :: IO ()
part1 = do
    contents <- readFile "input.txt"
    let lin = lines contents
    let f = map (\x -> fromJust $ find isDigit x) lin
    let l = map (\x -> fromJust $ find isDigit $ reverse x) lin
    let n = map (\(a, b) -> read [a,b] :: Int) (zip f l)
    let s = sum n
    print s

strToNum :: String -> Maybe Char
strToNum "one"   = Just '1'
strToNum "two"   = Just '2'
strToNum "three" = Just '3'
strToNum "four"  = Just '4'
strToNum "five"  = Just '5'
strToNum "six"   = Just '6'
strToNum "seven" = Just '7'
strToNum "eight" = Just '8'
strToNum "nine"  = Just '9'
strToNum "eno"   = Just '1'
strToNum "owt"   = Just '2'
strToNum "eerht" = Just '3'
strToNum "ruof"  = Just '4'
strToNum "evif"  = Just '5'
strToNum "xis"   = Just '6'
strToNum "neves" = Just '7'
strToNum "thgie" = Just '8'
strToNum "enin"  = Just '9'
strToNum "1" = Just '1'
strToNum "2" = Just '2'
strToNum "3" = Just '3'
strToNum "4" = Just '4'
strToNum "5" = Just '5'
strToNum "6" = Just '6'
strToNum "7" = Just '7'
strToNum "8" = Just '8'
strToNum "9" = Just '9'
strToNum _ = Nothing

numStrs :: [String]
numStrs =
        [ "one"
        , "two"
        , "three" 
        , "four"  
        , "five"  
        , "six"   
        , "seven" 
        , "eight" 
        , "nine"
        , "1"
        , "2"
        , "3"
        , "4"
        , "5"
        , "6"
        , "7"
        , "8"
        , "9"
        ]

numStrsRev :: [String]
numStrsRev = map reverse numStrs

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

getClosest :: (Maybe Int, Maybe Char) -> (Int, Maybe Char) -> (Int, Maybe Char)
getClosest (sI, sV) (accI, accV)
    | isJust sI && fromJust sI < accI = (fromJust sI, sV)
    | otherwise = (accI, accV)

getFirstNum :: String -> [String] -> Char
getFirstNum xs predicates =
    let (i, v) = foldr (\x acc -> getClosest (findString x xs, strToNum x) acc) (length xs, Nothing) predicates
    in fromJust v

part2 :: IO ()
part2 = do
    contents <- readFile "input.txt"
    let lins = lines contents
    let f = map (\x -> getFirstNum x numStrs) lins
    let l = map (\x -> getFirstNum (reverse x) numStrsRev) lins
    let n = map (\(a, b) -> read [a,b] :: Int) (zip f l)
    let s = sum n
    print s

main :: IO ()
main = do
    part1
    part2
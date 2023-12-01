import Data.Char
import Data.Maybe
import Data.List

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let lines' = lines contents
    let firstDigits = map (\x -> fromJust $ find isDigit x) lines'
    let lastDigits = map (\x -> fromJust $ find isDigit $ reverse x) lines'
    let numbers = zipWith (\a b -> read [a, b] :: Int) firstDigits lastDigits
    let sum' = sum numbers
    print sum'
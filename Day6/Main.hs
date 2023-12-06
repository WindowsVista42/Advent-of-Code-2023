module Main where

import Data.Char
import qualified Util
import qualified Data.Text as T
import qualified Data.Vector as V

solve :: V.Vector Int -> V.Vector Int -> Int
solve times distances =
    foldl foldFn 1 $ V.zip times distances where
        foldFn acc (time, dist) =
            acc * foldl (foldFn' time dist) 0 [0..time] where
                foldFn' time' dist' acc' t =
                    if ((time' - t) * t) > dist' then
                        acc' + 1
                    else
                        acc'

main :: IO ()
main = do
    lines' <- Util.readFile "Day6/input.txt"

    let times     = V.fromList $ Util.parseIntsOrZeros $ T.drop 5 $ head lines'
    let distances = V.fromList $ Util.parseIntsOrZeros $ T.drop 9 $ last lines'

    let times'     = V.fromList $ Util.parseIntsOrZeros $ T.filter isNumber $ head lines'
    let distances' = V.fromList $ Util.parseIntsOrZeros $ T.filter isNumber $ last lines'

    print $ solve times distances
    print $ solve times' distances'
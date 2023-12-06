module Util where

import Data.Maybe
import Data.Either

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Read as T.Read

skipLine :: [T.Text] -> [T.Text]
skipLine [] = []
skipLine (_:xs) = xs

skipEmptyLines :: [T.Text] -> [T.Text]
skipEmptyLines = dropWhile (==T.empty)

spanNonEmptyLines :: [T.Text] -> ([T.Text], [T.Text])
spanNonEmptyLines = span (/=T.empty)

splitOnSpace :: T.Text -> [T.Text]
splitOnSpace = T.splitOn $ T.pack " "

parseInt :: T.Text -> Maybe Int
parseInt x =
    let v = T.Read.decimal x
    in
        if isRight v then
            Just $ fst $ fromRight (undefined, T.empty) v
        else
            Nothing

parseInts :: T.Text -> [Maybe Int]
parseInts xs =
    parseInt <$> splitOnSpace xs

parseIntsOrZeros :: T.Text -> [Int]
parseIntsOrZeros xs =
    parseIntOrZero <$> splitOnSpace xs

parseIntOrZero :: T.Text -> Int
parseIntOrZero x = fromMaybe 0 $ parseInt x

linesAsText :: String -> [T.Text]
linesAsText lines' = T.pack <$> lines lines'

readFile :: String -> IO [T.Text]
readFile filename = do
    contents <- Prelude.readFile filename
    return $ linesAsText contents

whenMaybe :: Bool -> a -> Maybe a
whenMaybe False _ = Nothing
whenMaybe True a = Just a
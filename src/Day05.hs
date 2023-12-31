module Day05 (part1) where

import Data.List
import Data.List.Split

data AlmanacRange = AlmanacRange { dstStart :: Integer
                                 , srcStart :: Integer
                                 , len :: Integer }

data AlmanacMap = AlmanacMap String [AlmanacRange]

isInRange :: Integer -> AlmanacRange -> Bool
isInRange n AlmanacRange{srcStart = start, len=l} = n >= start && n <= start + l - 1

getDstFromMap :: AlmanacMap -> Integer -> Integer
getDstFromMap (AlmanacMap _ []) n = n
getDstFromMap (AlmanacMap _ map) n = case find (isInRange n) map of
                        Just r -> dstStart r + (n - srcStart r)
                        Nothing -> n

parseRange :: String -> AlmanacRange
parseRange = (\[dst,src,l] -> AlmanacRange { dstStart = dst
                                           , srcStart = src
                                           , len = l } ) . map read . words

parseMap :: [String] -> AlmanacMap
parseMap (desc:ranges) = AlmanacMap desc (map parseRange ranges)

findInMaxDepth :: [AlmanacMap] -> Integer -> Integer
findInMaxDepth ms n = foldl (flip getDstFromMap) n ms

part1 = do
    content <- splitOn [""] . lines <$> readFile "inputs/day05.txt"
    let seeds = map read $ words $ last $ splitOn ": " $ (head . head) content
    let maps = map parseMap $ tail content
    return (foldl1 min $ map (findInMaxDepth maps) seeds)

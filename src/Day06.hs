module Day06 (part1, part2) where

import Data.Bifunctor
import Data.List

g :: (Floating a, RealFrac a, Integral b) => a -> a -> (b,b)
g t s = bimap (floor . (+1)) (ceiling . subtract 1) ((t - sqrt (t^2 - 4*s)) / 2, (t + sqrt (t^2 - 4*s)) / 2)

parseRaces :: (Num a, Read a) => String -> [(a,a)]
parseRaces i = case (map (map read) . map (drop 1 . words) . lines) i of
                [time, recordDist] -> zip time recordDist 
                _ -> []

parseLongRace :: (Num a, Read a) => String -> (a,a)
parseLongRace i = case (map (read . intercalate "") . map (drop 1 . words) . lines) i of
                [time, recordDist] -> (time, recordDist)
                _ -> (0,0) 

part1 = product . map ((+1) . abs . uncurry (-)) . map (uncurry g) . parseRaces <$> readFile "inputs/day06.txt"
part2 = (+1) . abs . uncurry (-) . uncurry g . parseLongRace <$> readFile "inputs/day06.txt"

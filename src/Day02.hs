module Day02 (part1, part2) where

import Data.List.Split
import Data.Function (on)

data CubeSet = CubeSet { red :: Int, green :: Int, blue :: Int } deriving (Show)
isAnyFieldBigger :: CubeSet -> CubeSet -> Bool
isAnyFieldBigger lhs rhs = red lhs > red rhs || green lhs > green rhs || blue lhs > blue rhs

updateCubeSet :: CubeSet -> (Int, String) -> CubeSet
updateCubeSet old (n, color)
    | color == "red" = old { red = n }
    | color == "green" = old { green = n }
    | color == "blue" = old { blue = n }
    | otherwise = old

parseCubeSet :: String -> CubeSet
parseCubeSet input = foldl updateCubeSet CubeSet {red=0, green=0, blue=0} $ 
                     map (convertToTuple . splitOn " ") $ 
                     splitOn ", " input
    where convertToTuple arr = ((read . head) arr, last arr)

parseLine :: String -> [CubeSet]
parseLine input = map parseCubeSet $ splitOn "; " $ last $ splitOn ": " $ input

parse :: String -> [[CubeSet]] 
parse = map parseLine . lines

isGamePossible :: CubeSet -> [CubeSet] -> Bool
isGamePossible maxSet = foldl (\acc s -> if acc == True then not $ s `isAnyFieldBigger` maxSet else acc) True

updateMaxCubeSet :: CubeSet -> CubeSet -> CubeSet
updateMaxCubeSet maxSet curr = CubeSet { red=(max `on` red) maxSet curr
                                       , green=(max `on` green) maxSet curr
                                       , blue=(max `on` blue) maxSet curr }

gameMaxCubeSet :: [CubeSet] -> CubeSet
gameMaxCubeSet = foldl1 updateMaxCubeSet

maxCubeSet :: CubeSet
maxCubeSet = CubeSet {red=12, green=13, blue=14}

part1 = (sum . map fst . filter (isGamePossible maxCubeSet . snd) . zip [1..] . parse) <$> readFile "inputs/day02.txt"
part2 = sum . map (product . (\x -> ($ x) `map` [red, green, blue]) . gameMaxCubeSet) . parse <$> readFile "inputs/day02.txt"

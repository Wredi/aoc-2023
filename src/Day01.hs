module Day01 (part2) where

import Data.Char
import Data.List
import Data.Bifunctor
import Control.Monad

initDigitMap = [ ("one", '1')
            , ("two", '2') 
            , ("three", '3') 
            , ("four", '4') 
            , ("five", '5') 
            , ("six", '6') 
            , ("seven", '7') 
            , ("eight", '8')
            , ("nine", '9') ]

digitFromPrefix :: [(String, Char)] -> String -> Maybe Char
digitFromPrefix digitMap input@(c:_)
    | isDigit c = Just c 
    | otherwise = foldl (\acc (str, dig) -> if str `isPrefixOf` input then Just dig else acc) Nothing digitMap 

firstDigit :: [(String, Char)] -> String -> Maybe Char
firstDigit digitMap all@(_:cs)
    | currDigit == Nothing = firstDigit digitMap cs 
    | otherwise = currDigit
    where currDigit = digitFromPrefix digitMap all

parseLine :: String -> Maybe Int
parseLine line = read <$> sequence [leftNum, rightNum]
    where leftNum = firstDigit initDigitMap line 
          rightNum = firstDigit (map (first reverse) initDigitMap) (reverse line)

part2 = readFile "inputs/day01.txt" >>= (print . fmap sum . sequence . map parseLine . lines) 

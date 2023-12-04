import Data.List.Split
import Data.List
import Data.Bifunctor

type Card = ([Int],[Int])

parseCard :: String -> Card
parseCard line = bimap parseIntList parseIntList numberListsTuple
    where parseIntList = map read . words 
          numberListsTuple = (head numberLists, last numberLists)
          numberLists = (splitOn " | " . last . splitOn ": ") line 

parseCards :: [String] -> [Card]
parseCards = map parseCard 

getWinningNumbers :: Card -> [Int]
getWinningNumbers (w,curr) = intersect w curr 

getPoints :: [Int] -> Int
getPoints arr = if len == 0 then 0 else 2 ^ (len-1)
    where len = length arr

main = sum . map getPoints . map getWinningNumbers . parseCards . lines <$> readFile "input.txt"

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

getCardsCount :: [Card] -> Int -> Int
getCardsCount cs idx
    | idx >= length cs = 0
    | otherwise = len + (sum $ map (getCardsCount cs) $ take len $ [idx+1..])
    where len = (length . getWinningNumbers) c
          c = cs !! idx

getPoints :: [Int] -> Int
getPoints arr = if len == 0 then 0 else 2 ^ (len-1)
    where len = length arr

--main = sum . map getPoints . map getWinningNumbers . parseCards . lines <$> readFile "input.txt"
main = do
    cards <- parseCards . lines <$> readFile "input.txt"
    return (length cards + (sum $ map (getCardsCount cards) $ take (length cards) [0..]))

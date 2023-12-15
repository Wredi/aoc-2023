module Day07 (part2) where

import Data.List.Split 
import qualified Data.List as L
import Data.Function
import Data.Maybe
import Data.Bifunctor
import qualified Data.Map as Map

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight 
            | Nine | Ten | Queen | King | Ace deriving (Eq,Ord,Show)

data HandType = HighCard | OnePair | TwoPair
                | ThreeOfAKind | FullHouse | FourOfAKind 
                | FiveOfAKind deriving (Eq,Ord,Show)

data Hand = Hand HandType [Card] deriving (Show)

instance Eq Hand where
    (Hand t1 cs1) == (Hand t2 cs2) = (t1 == t2) && (cs1 == cs2)

instance Ord Hand where
    (Hand t1 cs1) `compare` (Hand t2 cs2) = if t1 /= t2 then t1 `compare` t2
                                            else cs1 `compare` cs2

parseCard :: Char -> Card
parseCard c = case c of
                'J' -> Joker
                '2' -> Two
                '3' -> Three
                '4' -> Four
                '5' -> Five
                '6' -> Six
                '7' -> Seven
                '8' -> Eight
                '9' -> Nine
                'T' -> Ten
                'Q' -> Queen
                'K' -> King
                'A' -> Ace
                _ -> undefined

parseCards :: String -> [Card]
parseCards = map parseCard

countOccurrences :: (Ord a) => [a] -> Map.Map a Int
countOccurrences = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty

changeFirstMatch :: (Eq a) => (a -> a) -> (a -> Bool) -> [a] -> [a]
changeFirstMatch _ _ [] = []
changeFirstMatch trans check (c:cs) 
    | check c = trans c:cs 
    | otherwise = c : changeFirstMatch trans check cs

handType :: [Card] -> HandType
handType cards
	| countList == [5] = FiveOfAKind
	| countList == [1,4] = FourOfAKind
	| countList == [2,3] = FullHouse
	| countList == [1,1,3] = ThreeOfAKind
	| countList == [1,2,2] = TwoPair
	| countList == [1,1,1,2] = OnePair
	| countList == [1,1,1,1,1] = HighCard
    | otherwise = error (show cards)
    where occurrencesMap = countOccurrences cards 
          occurrences = map snd . filter ((/=Joker) . fst) . Map.toList $ occurrencesMap
          jokerLength = fromMaybe 0 . Map.lookup Joker $ occurrencesMap  
          maxCountNum = maximum occurrences
          countList = if occurrences == [] then [jokerLength]
                      else L.sort . changeFirstMatch (+jokerLength) (==maxCountNum) $ occurrences

parseHand :: String -> Hand
parseHand cardChars = Hand (handType cards) cards
    where cards = parseCards cardChars
    
parseHandWithBid :: (Integral a, Read a) => String -> (Hand,a)
parseHandWithBid input = bimap parseHand read (cardChars, bid)
    where [cardChars, bid] = words input

parseHandsWithBid :: (Integral a, Read a) => [String] -> [(Hand,a)]
parseHandsWithBid = map parseHandWithBid

part2 = (sum . map (uncurry (*)) . zip [1..] . map snd . L.sortBy (compare `on` fst) . parseHandsWithBid . lines) <$> readFile "inputs/day07.txt"

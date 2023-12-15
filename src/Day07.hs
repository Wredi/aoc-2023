module Day07 (part1) where

import Data.List.Split 
import Data.List
import Data.Function
import Data.Bifunctor

data Card = Two | Three | Four | Five | Six | Seven | Eight 
            | Nine | Ten | Jack | Queen | King | Ace deriving (Eq,Ord,Show)

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
                '2' -> Two
                '3' -> Three
                '4' -> Four
                '5' -> Five
                '6' -> Six
                '7' -> Seven
                '8' -> Eight
                '9' -> Nine
                'T' -> Ten
                'J' -> Jack
                'Q' -> Queen
                'K' -> King
                'A' -> Ace
                _ -> undefined

parseCards :: String -> [Card]
parseCards = map parseCard

handType :: [Card] -> HandType
handType cards
	| 5 `isCountEq` 5 = FiveOfAKind
	| 4 `isCountEq` 4 && 1 `isCountEq` 1 = FourOfAKind
	| 3 `isCountEq` 3 && 2 `isCountEq` 2 = FullHouse
	| 3 `isCountEq` 3  && 1 `isCountEq` 2 = ThreeOfAKind
	| 2 `isCountEq` 4 && 1 `isCountEq` 1 = TwoPair
	| 2 `isCountEq` 2 && 1 `isCountEq` 3 = OnePair
	| 1 `isCountEq` 5 = HighCard
    where countList = map cardCount cards
          cardCount c = (length . filter (==True) . map (==c)) cards
          isCountEq num reqNum = (length . filter (==num)) countList == reqNum

parseHand :: String -> Hand
parseHand cardChars = Hand (handType cards) cards
    where cards = parseCards cardChars
    
parseHandWithBid :: (Integral a, Read a) => String -> (Hand,a)
parseHandWithBid input = bimap parseHand read (cardChars, bid)
    where [cardChars, bid] = words input

parseHandsWithBid :: (Integral a, Read a) => [String] -> [(Hand,a)]
parseHandsWithBid = map parseHandWithBid

part1 = (sum . map (uncurry (*)) . zip [1..] . map snd . sortBy (compare `on` fst) . parseHandsWithBid . lines) <$> readFile "inputs/day07.txt"

import Data.Char

type Coord = (Int, Int)
type IndexedNumber = (Coord, String)
type IndexedString = [(Coord, Char)]

parseNumber :: IndexedString -> (String, IndexedString)
parseNumber [] = ([], [])
parseNumber all@((p,c):cs)
    | isDigit c = (c:num, rest)
    | otherwise = ([], all)
    where (num, rest) = parseNumber cs

allNumbersInString :: IndexedString -> [IndexedNumber]
allNumbersInString [] = []
allNumbersInString all@((p,c):cs)
    | isDigit c = (p, number) : allNumbersInString rest
    | otherwise = allNumbersInString cs
    where (number, rest) = parseNumber all 

getAllNumbers :: [[Char]] -> [IndexedNumber]
getAllNumbers arr = concat [allNumbersInString [((x,y),c) | (x,c) <- zip [0..] line] | (y, line) <- zip [0..] arr] 

safeArrCheck :: (Eq a) => [[a]] -> (a -> Bool) -> Coord -> Bool
safeArrCheck arr check (x,y) 
    | y >= length arr || y < 0 = False
    | x >= length row || x < 0 = False
    | otherwise = check (row !! x)
    where row = arr !! y

isAdjacentToSymbol :: [[Char]] -> IndexedNumber -> Bool
isAdjacentToSymbol table ((i,j),num) = or [safeArrCheck table checkFunc (x,y) | x <- [i-1..i+length num], y <- [j-1..j+1]]
    where checkFunc c = (not $ isDigit c) && (c /= '.')

getPartNumbers :: [[Char]] -> [Int]
getPartNumbers arr = (map (read . snd) . filter (isAdjacentToSymbol arr) . getAllNumbers) arr

main = sum . getPartNumbers . lines <$> readFile "input.txt"

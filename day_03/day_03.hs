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

adjacentCheck :: [[Char]] -> (Char -> Bool) -> IndexedNumber -> Bool
adjacentCheck table func ((i,j),num) = or [safeArrCheck table func (x,y) | x <- [i-1..i+length num], y <- [j-1..j+1]]

isAdjacentToSymbol :: [[Char]] -> IndexedNumber -> Bool
isAdjacentToSymbol table = adjacentCheck table checkFunc
    where checkFunc c = (not $ isDigit c) && (c /= '.')

findStarCoords :: [[Char]] -> [Coord]
findStarCoords table = map fst . filter ((=='*') . snd) . concat $ coordsTable
    where idxTable = zip [0..] table
          coordsTable = map (\(y,line) -> map (\(x,c) -> ((x,y),c)) $ zip [0..] line) idxTable 

getPartNumbers :: [[Char]] -> [Int] 
getPartNumbers arr = (map (read . snd) . filter (isAdjacentToSymbol arr) . getAllNumbers) arr

main = do
    input <- readFile "input.txt"
    let table = lines input
    let numbers = filter (adjacentCheck table (=='*')) $ getAllNumbers table
    let starCoords = findStarCoords table
    print $ (sum . map (product . map (read . snd)) . filter (\x -> length x == 2) . map (getAdjacentNumbers numbers)) starCoords
    where getAdjacentNumbers numbers p = filter (isNumberAdjacent p) numbers
          isNumberAdjacent (xP,yP) ((xN,yN), num) = yN >= yP-1 && yN <= yP+1 
                                                 && xP >= xN-1 && xP <= xN + length num 

--main = sum . getPartNumbers . lines <$> readFile "input.txt"

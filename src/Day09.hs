module Day09 (part1) where

diff :: (Num a) => [a] -> [a]
diff xs = zipWith (-) (tail xs) xs

nextSeqValue :: [Integer] -> Integer
nextSeqValue [] = error "not possible"
nextSeqValue seq
    | all (==0) seq = 0
    | otherwise = last seq + nextSeqValue (diff seq)

part1 = (sum . map nextSeqValue . map (map read . words) . lines) <$> readFile "inputs/day09.txt"

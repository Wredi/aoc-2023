module Day09 (part1,part2) where

diff :: (Num a) => [a] -> [a]
diff xs = zipWith (-) (tail xs) xs

extrapolateSeq :: ([Integer] -> Integer) 
               -> (Integer -> Integer -> Integer) 
               -> [Integer] -> Integer 
extrapolateSeq element op seq
    | all (==0) seq = 0
    | otherwise = element seq `op` extrapolateSeq element op (diff seq)

nextSeqValue :: [Integer] -> Integer
nextSeqValue = extrapolateSeq last (+)

prevSeqValue :: [Integer] -> Integer
prevSeqValue = extrapolateSeq head (-)

part1 = (sum . map nextSeqValue . map (map read . words) . lines) <$> readFile "inputs/day09.txt"
part2 = (sum . map prevSeqValue . map (map read . words) . lines) <$> readFile "inputs/day09.txt"

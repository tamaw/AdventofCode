import Data.List

input :: [Int]
input = [197487..673251]

part1 :: Int
part1 = length $ findpass input

findpass :: [Int] -> [Int]
findpass s = filter match s

match :: Int -> Bool
match n = neverDec digits && hasAdj digits && isSixDigits n
    where 
        digits = getDigits n

isSixDigits :: Int -> Bool
isSixDigits n = n >= 100000 && n < 1000000


getDigits :: Int -> [Int]
getDigits num = [num `div` 10^p `rem` 10 | p <- [5,4..0]]

hasAdj :: [Int] -> Bool
hasAdj = not . any ((== 2) . length) . group

neverDec :: [Int] -> Bool
neverDec (x:y:rest) = x <= y && neverDec (y:rest)
neverDec [x] = True
neverDec [] = True

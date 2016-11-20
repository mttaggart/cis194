import Data.Char

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = [fromIntegral (digitToInt x) | x <- show n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse  (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1,2])

sumDigits :: [Integer] -> Integer
sumDigits n = sum ((=<<) toDigits n)

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther $ toDigits n) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c =
  stepOneMoves ++ stepTwoMoves ++ stepThreeMoves
  where
    stepOneMoves = hanoi (n-1) a c b
    stepTwoMoves = hanoi 1 a b c
    stepThreeMoves = hanoi (n-1) c b a

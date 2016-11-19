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

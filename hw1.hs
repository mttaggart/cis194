toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = map (read . return) . show n


toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse  toDigits n

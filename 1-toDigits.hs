toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' = zipWith (*) $ cycle [1, 2]

doubleEveryOther = reverse . doubleEveryOther' . reverse

sumDigits :: [Integer] -> Integer
sumDigits n = sum . concat $ map toDigits n

checkSum :: Integer -> Integer
checkSum n = (sumDigits . doubleEveryOther $ toDigits n) `mod` 10

--validate :: Integer -> Bool
validate :: Integer -> Bool
validate n = checkSum n == 0

main = putStrLn "Sup"

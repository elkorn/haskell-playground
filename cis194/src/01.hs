import Data.Char (digitToInt, isDigit)
import qualified Data.List as List

main = do
    print $ toDigits "12345"
    print $ toDigits "-12345"
    print $ toDigits "0"
    print $ toDigitsRev "12345"
    print $ doubleEveryOther . toDigits $ "12345"
    print $ doubleEveryOther . toDigits $ "1234"
    print $ doubleEveryOther . toDigits $ "12"
    print $ doubleEveryOther . toDigits $ ""
    print $ validateCreditCard creditCardNumber1
    print $ validateCreditCard correctCreditCardNumber
    print $ sumDigits . doubleEveryOther . toDigits $ "1386"



creditCardNumber1 :: String
creditCardNumber1 = "3333000030303333"

creditCardNumber2 :: String
creditCardNumber2 = "2222222222222222"

correctCreditCardNumber :: String
correctCreditCardNumber = "4012888888881881"

validateCreditCard :: String -> Bool
validateCreditCard = validate . sumDigits . doubleEveryOther . toDigits

toDigits :: String -> [Integer]
toDigits "" = []
toDigits (x:xs)
  | not (isDigit x) = []
  | x == '0' = []
  | otherwise = map (toInteger . digitToInt) (x:xs)

toDigitsRev :: String -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = let
  doubleEveryOther' (x:y:ys) = x:2*y:doubleEveryOther' ys
  doubleEveryOther' [x] = [x]
  doubleEveryOther' [] = []
  in List.reverse . doubleEveryOther' . List.reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10)

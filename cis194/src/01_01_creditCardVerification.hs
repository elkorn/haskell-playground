import Data.Char (digitToInt, isDigit)
import qualified Data.List as List

main =
  do print $
       toDigits "12345"
     print $
       toDigits "-12345"
     print $
       toDigits "0"
     print $
       toDigitsRev "12345"
     print $ doubleEveryOther . toDigits $ "12345"
     print $ reverse . doubleEveryOther . toDigits . reverse $ "12345"
     print $ doubleEveryOther . toDigits $ "12"
     print $ doubleEveryOther . toDigits $ ""
     print $ validateCreditCard creditCardNumber1
     print $ validateCreditCard correctCreditCardNumber
     print $ sumEveryOtherDigit . doubleEveryOther . reverse . toDigits $
       correctCreditCardNumber
     print $ sumEveryOtherDigit . doubleEveryOther . toDigits $ "1386"
     print $ doubleEveryOther . toDigitsRev $ "1386"
     print $
       sumEveryOtherDigit [1,3,5,6,8,7]

creditCardNumber1 :: String
creditCardNumber1 = "3333000030303333"

correctCreditCardNumber :: String
correctCreditCardNumber = "4012888888881829"

validateCreditCard :: String -> Bool
validateCreditCard = validate . sumEveryOtherDigit . doubleEveryOther . toDigits

toDigits :: String -> [Integer]
toDigits "" = []
toDigits (x:xs)
  | not (isDigit x) = []
  | x == '0' = []
  | otherwise =
    map (toInteger . digitToInt)
        (x : xs)

toDigitsRev :: String -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:ys) = x : 2 * y : doubleEveryOther ys
doubleEveryOther [x] = [x]
doubleEveryOther [] = []

sumEveryOtherDigit :: [Integer] -> Integer
sumEveryOtherDigit digs =
  let withIndex =
        zipWith (\ind dig -> (ind,dig))
                [0 ..]
                digs
  in foldl (+) 0 $
     map snd $
     filter (\(ind,_) -> ind `mod` 2 == 1) withIndex

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10)

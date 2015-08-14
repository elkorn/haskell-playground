import Data.Char
import qualified Data.List as L
import Data.Function (on)

disp :: (String, Bool) -> String
disp (name,result) = name ++ " -> " ++ show result

checkCharProps :: Char -> [(String, Bool)]
checkCharProps c = let check :: Char -> (String, Char -> Bool) -> (String, Bool)
                       check ch (name,fn) = (name, fn ch)
                       props = [ ("isControl", isControl)
                               , ("isSpace", isSpace)
                               , ("isLower", isLower)
                               , ("isUpper", isUpper)
                               , ("isAlpha", isAlpha)
                               , ("isAlphaNum", isAlphaNum)
                               , ("isPrint", isPrint)
                               , ("isDigit", isDigit)
                               , ("isOctDigit", isOctDigit)
                               , ("isHexDigit", isHexDigit)
                               , ("isLetter", isLetter)
                               , ("isMark", isMark)
                               , ("isNumber", isNumber)
                               , ("isPunctuation", isPunctuation)
                               , ("isSymbol", isSymbol)
                               , ("isSeparator", isSeparator)
                               , ("isAscii", isAscii)
                               , ("isLatin1", isLatin1)
                               , ("isAsciiUpper", isAsciiUpper)
                               , ("isAsciiLower", isAsciiLower)]
    in map (check c) props

checkGeneralCategories :: [Char] -> [GeneralCategory]
checkGeneralCategories = map generalCategory

caesarEnc :: Int -> String -> String
caesarEnc shift msg  = let
  ords = map ord msg
  shifted = map (+ shift) ords
  in map chr shifted

caesarDec :: Int -> String -> String
caesarDec shift = caesarEnc $ negate shift

disp' :: (Show a)
      => [a] -> String
disp' as = unlines $
    map show as

main :: IO ()
main = do
    putStr $
        unlines $
        map disp $
        checkCharProps '4'
    print $
        all isAlphaNum "test123"
    print $
        all isAlphaNum "hey hello"
    -- imitating the functionality of `words`
    print $
        words "hey there, hello!"
    print $
        L.groupBy ((==) `on` isSpace) "hey there, hello!"
    print $
        filter (not . any isSpace) .
        L.groupBy ((==) `on` isSpace) $
        "hey there, hello!"
    putStr $
        let str = "Hey there 123!"
        in disp' $
           zip (checkGeneralCategories str) str
    print $
        map digitToInt "34538"
    print $
        map digitToInt "BADA55"
    print $
        ord 'a'
    print $
        chr $
        ord 'a'
    print $
        map ord ['a' .. 'e']
    print $ caesarEnc 12 "Hi there dude!"
    print $ caesarDec 12 $ caesarEnc 12 "Hi there dude!"

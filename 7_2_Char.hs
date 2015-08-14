import Data.Char
import Data.List

disp :: (String, Bool) -> String
disp (name, result) = name ++ " -> " ++ show result

checkCharProps :: Char -> [(String, Bool)]
checkCharProps c = let
    check :: Char -> (String, Char -> Bool) -> (String, Bool)
    check c (name, fn) = (name, fn c)
    props = [
        ("isControl", isControl),
        ("isSpace", isSpace),
        ("isLower", isLower),
        ("isUpper", isUpper),
        ("isAlpha", isAlpha),
        ("isAlphaNum", isAlphaNum),
        ("isPrint", isPrint),
        ("isDigit", isDigit),
        ("isOctDigit", isOctDigit),
        ("isHexDigit", isHexDigit),
        ("isLetter", isLetter),
        ("isMark", isMark),
        ("isNumber", isNumber),
        ("isPunctuation", isPunctuation),
        ("isSymbol", isSymbol),
        ("isSeparator", isSeparator),
        ("isAscii", isAscii),
        ("isLatin1", isLatin1),
        ("isAsciiUpper", isAsciiUpper),
        ("isAsciiLower", isAsciiLower)]
    in map (check c) props

main :: IO()
main = do
  putStr $ unlines $ map disp $ checkCharProps '4'

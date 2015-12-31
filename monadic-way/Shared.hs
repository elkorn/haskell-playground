module MonadicWay.Shared where

data Term
    = Con Int
    | Add Term
          Term
    deriving (Show)

type Exception = String
type O = String
type Output = String

formatLine :: Term -> Int -> Output
formatLine term value = "eval (" ++ show term ++ ") <= " ++ show value ++ " - "

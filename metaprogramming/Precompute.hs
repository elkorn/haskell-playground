module Precompute where

import Language.Haskell.TH

import Domain

intToPat :: Int -> Pat
intToPat = LitP . IntegerL . toInteger

precompute :: [Int] -> DecsQ
precompute xs = do
  let name               = mkName "lookupTable"
      -- function argument patterns
  let patterns           = map intToPat xs
      -- precoumputed function results
  let fnBodies           = map precomputeInteger xs
      -- create clauses assigning precomputed results to argument patterns
  let precomputedClauses = zipWith (\body pattern -> Clause [pattern] (NormalB body) []) fnBodies patterns
     -- generate the final `lookupTable x = bigBadMathProblem x` clause.
  let x'                  = mkName "x"
  let appBody             = AppE (VarE (mkName "bigBadMathProblem")) (VarE x')
  let lastClause          = [Clause [VarP x'] (NormalB appBody) []]
  -- data Clause = Clause [Pat] Body [Dec]
  return [FunD name (precomputedClauses ++ lastClause)]

precomputeInteger :: Int -> Exp
precomputeInteger = LitE . DoublePrimL . toRational . bigBadMathProblem

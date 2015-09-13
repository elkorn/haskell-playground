module Main where

import Test.Framework.Runners.Console (defaultMain)
import FixUnit

main =
  defaultMain $
  [FixUnit.tests]

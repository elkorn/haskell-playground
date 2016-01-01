module CommonPatterns where

import CommonPatterns.LocalReader

import Control.Monad.Reader (runReader)

main :: IO ()
main = do
  print $ runReader localExample "Fred"

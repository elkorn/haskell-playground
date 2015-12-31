module CountEntries where

import qualified CountEntries.Classic as C
import qualified CountEntries.Monadic as M

import Control.Monad.Writer (runWriterT, execWriterT)


main :: IO ()
main = do
  recordedData1 <- C.countEntries "."
  ((), recordedData2) <- runWriterT $ M.countEntries "."
  recordedData3 <- execWriterT $ M.countEntries "."
  print $ show recordedData1
  print $ show recordedData2
  print $ show recordedData3

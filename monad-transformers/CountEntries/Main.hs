module CountEntries where

import qualified CountEntries.Classic as C
import qualified CountEntries.Monadic as M
import qualified CountEntries.UglyStack as US

import Control.Monad.Writer (runWriterT, execWriterT)

main :: IO ()
main = do
  recordedData1 <- C.countEntries "."
  ((), recordedData2) <- runWriterT $ M.countEntries "."
  recordedData3 <- execWriterT $ M.countEntries "."
  recordedData4 <- US.runApp (US.constrainedCount ".") 2
  recordedData5 <- US.runApp (US.constrainedCount ".") 1
  recordedData6 <- US.runApp1 (US.constrainedCount1 ".") 2
  recordedData7 <- US.runApp1 (US.constrainedCount1 ".") 1
  print $ show recordedData1
  print $ show recordedData2
  print $ show recordedData3
  print $ show recordedData4
  print $ show recordedData5
  print $ show recordedData6
  print $ show recordedData7

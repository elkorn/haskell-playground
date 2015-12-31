module MonadicWay.MyStateT where

-- `StateT` USED FOR ALL CROSS-CUTTING CONCERNS, STD LIBRARY

import Control.Monad.State.Lazy
import MonadicWay.Shared


data EvalST = State { getIOS :: IOStack
                    , getDebug :: Debug
                    , getCount :: Int
                    } deriving Show

data MT a = Fail Exception
          | Done {unpackDone :: a}
            deriving Show

type Eval s a = StateT s MT a

mkMT :: a -> MT a
mkMT value = Done value

bindMT :: MT a -> (a -> MT b) -> MT b
bindMT monad doNext = case monad of
  Fail e -> Fail e
  Done a -> doNext a

instance Monad MT where
  return = mkMT
  (>>=) = bindMT

instance Functor MT where
  fmap _ (Fail e) = Fail e
  fmap f (Done a) = Done $ f a

emptyState = State [] [] 0

stopExecT :: Output -> Eval EvalST Int
stopExecT err = lift $ Fail err

catchExecT :: Output -> Eval EvalST ()
catchExecT err = do
  st <- get
  let state = getCount st
  let debug = getDebug st
  let outStack = getIOS st
  let msg = "Debug msg at Iteration " ++ show state ++ ": " ++ err
  put $ State outStack (msg:debug) state

printT :: Output -> Eval EvalST ()
printT out = do
  st <- get
  let state = getCount st
  let debug = getDebug st
  let outStack = getIOS st
  let msg = show state ++ ": - " ++ out
  put $ State (msg:outStack) debug state

incTcounter :: Eval EvalST ()
incTcounter = do
  st <- get
  let state = getCount st
  let debug = getDebug st
  let outStack = getIOS st
  put $ State outStack debug (state + 1)

evaluateT :: Int -> Output -> Eval EvalST Int
evaluateT result output = do
  incTcounter
  printT output
  case result of
    42 -> do
      catchExecT "The ultimate answer. Goodbye!"
      return result
    11 -> stopExecT "Number 11 is forbidden."
    otherwise -> return result

evalT :: Term -> Eval EvalST Int
evalT con@(Con a) = evaluateT a $ formatLine con a
evalT add@(Add t u) = do
  a <- evalT t
  b <- evalT u
  let result = a + b
  let output = formatLine add result
  evaluateT result output

printAll :: [String] -> IO ()
printAll = mapM_ putStrLn

eval :: Term -> IO ()
eval exp = case execStateT (evalT exp) emptyState of
  Fail e -> putStrLn e
  done@(Done (State outStack debug state)) -> do
    printAll $ reverse outStack
    print $ unpackDone done
    case debug of
      [] -> putStrLn $ "Iterations: " ++ show state
      _ -> do
        printAll $ reverse debug
        putStrLn $ "Iterations: " ++ show state

runEvalT :: Term -> String
runEvalT exp = case runStateT (evalT exp) emptyState of
  Fail e -> e
  Done (result, State outStack debug state) -> "Result = " ++ show result ++
                                               "; Iteration = " ++ show state ++
                                               "; Output = " ++ show outStack ++
                                               " - Errors = " ++ show debug
main :: IO ()
main = do
    let term = Add
                (Con 5)
                (Con 6)
    let term42a = Add
                (Con 42)
                (Con 0)
    let term42b = Add
                (Con 0)
                (Con 42)
    let term42c = Add
                (Con 40)
                (Con 2)
    mapM_ eval [term, term42a, term42b, term42c]

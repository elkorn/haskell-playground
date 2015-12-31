module MonadicWay.Two where

import MonadicWay.Shared

import Control.Applicative

newtype StateT s m a = StateT
    { runStateT :: s -> m (a, s)
    }

instance Monad m => Monad (StateT s m) where
    return a = StateT
            (\s ->
                  return (a, s))
    -- `k` looks as the actual transformer function
    -- the `~` signifies an irrefutable (lazy) pattern - it's a thunk
    -- `>>=` transforms between m1 and m2 (they can have different types)
    StateT m1 >>= k = StateT
            (\s ->
                  do ~(a,s1) <- m1 s
                     let StateT m2 = k a
                     m2 s1)
-- Execute a stateful computation, return both the result and the final state
-- in a monad.
runState :: s -> StateT s m a -> m (a, s)
runState initialState (StateT m) = m initialState

-- Execute a stateful computation, drop the final state, return the result
-- in a monad.
evalState :: Functor m
          => s -> StateT s m a -> m a
evalState initialState m = fmap fst (runState initialState m)

-- Execute a stateful computation, drop the result, return the state in a monad.
execState :: Functor m
          => s -> StateT s m a -> m s
execState initialState m = fmap snd (runState initialState m)

lift :: Monad m
     => m a -> StateT s m a
lift m = StateT
        (\state ->
              m >>=
              \result ->
                   return (result, state))

-- `StateT` AS A COUNTER, MONADIC EVALUATOR WITH OUTPUT AND EXCEPTIONS

data MTa a = Fail Exception
           | Done { unpackDone :: (a, O) }
             deriving Show

mkMTa :: a -> MTa a
mkMTa value = Done (value, "")

bindMTa :: MTa a -> (a -> MTa b) -> MTa b
bindMTa monad doNext = case monad of
  Fail err -> Fail err
  Done (oldValue, oldState) -> case (doNext oldValue) of
    Fail err -> Fail err
    Done (newValue, newState) -> Done (newValue, oldState ++ newState)

instance Monad MTa where
  return = mkMTa
  m >>= f = bindMTa m f
  -- or
  -- (>>=) = bindMTa

instance Functor MTa where
  fmap _ (Fail a) = Fail a
  fmap f (Done (result, state)) = Done ((f result), state)

raiseTa_SIOE :: O -> StateT Int MTa a
raiseTa_SIOE err = lift $ Fail err

printTa_SIOE :: O -> StateT Int MTa ()
printTa_SIOE out = lift $ Done ((), out)

incTaState :: StateT Int MTa ()
incTaState = StateT $ \state -> return ((), state + 1)
                      
evalTa_SIOE :: Term -> StateT Int MTa Int
evalTa_SIOE con@(Con a) = do
  incTaState
  let out = formatLine con a
  printTa_SIOE out
  if a == 42
     then raiseTa_SIOE $ out ++ "The ultimate answer. Goodbye!"
     else return a
evalTa_SIOE add@(Add t u) = do
  a <- evalTa_SIOE t
  b <- evalTa_SIOE u
  let result = a + b
  let out = formatLine add result
  printTa_SIOE out
  if result == 42
     then raiseTa_SIOE $ out ++ "The ultimate answer. Goodbye!"
     else return result

runEvalRunTa1 :: Term -> String
runEvalRunTa1 exp = case runStateT (evalTa_SIOE exp) 0 of
  Fail e -> e
  Done (~(result, state), output) -> "Result = " ++ show result ++
                                     "; Iteration = " ++ show state ++
                                     "; Output = " ++ output

runEvalRunTa2 :: Term -> String
runEvalRunTa2 exp = case runState 0 (evalTa_SIOE exp) of
  Fail e -> e
  Done (~(result, state), output) -> "Result = " ++ show result ++
                                     "; Iteration = " ++ show state ++
                                     "; Output = " ++ output

runEvalEvalTa :: Term -> String
runEvalEvalTa exp = case evalState 0 (evalTa_SIOE exp) of
  Fail e -> e
  Done (result, output) -> "Result = " ++ show result ++
                           "; Output = " ++ output


runEvalExecTa :: Term -> String
runEvalExecTa exp = case execState 0 (evalTa_SIOE exp) of
  Fail e -> e
  Done (state, output) -> "Iteration = " ++ show state ++
                          "; Output = " ++ output
  
                                
main :: IO ()
main = do
    let term = Add (Con 5) (Con 6)
    let term42a = Add (Con 42) (Con 0)
    let term42b = Add (Con 0) (Con 42)
    let term42c = Add (Con 40) (Con 2)
    let evals = [runEvalRunTa1, runEvalRunTa2, runEvalEvalTa, runEvalExecTa]
    let terms = [term, term42a, term42b, term42c]
    mapM_ putStrLn $ evals <*> terms

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

data MTa a = FailTa Exception
           | DoneTa { unpackDoneTa :: (a, O) }
             deriving Show

mkMTa :: a -> MTa a
mkMTa value = DoneTa (value, "")

bindMTa :: MTa a -> (a -> MTa b) -> MTa b
bindMTa monad doNext = case monad of
  FailTa err -> FailTa err
  DoneTa (oldValue, oldState) -> case (doNext oldValue) of
    FailTa err -> FailTa err
    DoneTa (newValue, newState) -> DoneTa (newValue, oldState ++ newState)

instance Monad MTa where
  return = mkMTa
  m >>= f = bindMTa m f
  -- or
  -- (>>=) = bindMT

instance Functor MTa where
  fmap _ (FailTa a) = FailTa a
  fmap f (DoneTa (result, state)) = DoneTa ((f result), state)

raiseTa_SIOE :: O -> StateT Int MTa a
raiseTa_SIOE err = lift $ FailTa err

printTa_SIOE :: O -> StateT Int MTa ()
printTa_SIOE out = lift $ DoneTa ((), out)

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
  FailTa e -> e
  DoneTa (~(result, state), output) -> "Result = " ++ show result ++
                                     "; Iteration = " ++ show state ++
                                     "; Output = " ++ output

runEvalRunTa2 :: Term -> String
runEvalRunTa2 exp = case runState 0 (evalTa_SIOE exp) of
  FailTa e -> e
  DoneTa (~(result, state), output) -> "Result = " ++ show result ++
                                     "; Iteration = " ++ show state ++
                                     "; Output = " ++ output

runEvalEvalTa :: Term -> String
runEvalEvalTa exp = case evalState 0 (evalTa_SIOE exp) of
  FailTa e -> e
  DoneTa (result, output) -> "Result = " ++ show result ++
                           "; Output = " ++ output


runEvalExecTa :: Term -> String
runEvalExecTa exp = case execState 0 (evalTa_SIOE exp) of
  FailTa e -> e
  DoneTa (state, output) -> "Iteration = " ++ show state ++
                          "; Output = " ++ output
  
-- `StateT` TO KEEP OUTPUT AND COUNTER, MONADIC EVALUATOR WITH EXCEPTIONS

-- To count iterations properly, an analogue of MonadicState.One.Eval_SIOE
-- needs to be used here.
data MTb a = FailTb Exception
           | DoneTb { unpackDoneTb :: a }

type StateIO = (O, Int)

mkMTb :: a -> MTb a
mkMTb value = DoneTb value

bindMTb :: MTb a -> (a -> MTb b) -> MTb b
bindMTb monad doNext = case monad of
  FailTb e -> FailTb e
  DoneTb a -> doNext a

instance Monad MTb where
  return = mkMTb
  m >>= f = bindMTb m f

instance Functor MTb where
  fmap _ (FailTb err) = FailTb err
  fmap f (DoneTb value) = DoneTb $ f value

raiseTb_SIOE :: O -> StateT StateIO MTb a
raiseTb_SIOE err = lift (FailTb err)

-- Putting more responsibilities inside the `StateT` manipulation function.
printTb_SIOE :: O -> StateT StateIO MTb ()
printTb_SIOE out = StateT (\(output, state) -> return ((), (output ++ out, state)))

incTbState_SIOE :: StateT StateIO MTb ()
incTbState_SIOE = StateT (\(output, state) -> return ((), (output, state+1)))

evaluateTb_SIOE :: Int -> O -> StateT StateIO MTb Int
evaluateTb_SIOE result output = do
  printTb_SIOE output
  incTbState_SIOE
  if result == 42 then raiseTb_SIOE $ output ++ "Ultimate answer. Goodbye!"
                  else return result
  
evalTb_SIOE :: Term -> StateT StateIO MTb Int
evalTb_SIOE con@(Con a) = evaluateTb_SIOE a $ formatLine con a
evalTb_SIOE add@(Add t u) = do
  a <- evalTb_SIOE t
  b <- evalTb_SIOE u
  let result = a + b
  evaluateTb_SIOE result $ formatLine add result

runEvalRunTb1 :: Term -> String
runEvalRunTb1 exp = case runStateT (evalTb_SIOE exp) ("", 0) of
  FailTb err -> err
  DoneTb (result, ~(output, state)) -> "Result = " ++ show result ++
                                     "; Iteration = " ++ show state ++
                                     "; Output = " ++ output
  
runEvalRunTb2 :: Term -> String
runEvalRunTb2 exp = case runState ("", 0) (evalTb_SIOE exp) of
  FailTb err -> err
  DoneTb (result, ~(output, state)) -> "Result = " ++ show result ++
                                     "; Iteration = " ++ show state ++
                                     "; Output = " ++ output
runEvalEvalTb :: Term -> String
runEvalEvalTb exp = case evalState ("", 0) (evalTb_SIOE exp) of
  FailTb err -> err
  DoneTb result -> "Result = " ++ show result

runEvalExecTb :: Term -> String
runEvalExecTb exp = case execState ("", 0) (evalTb_SIOE exp) of
  FailTb err -> err
  DoneTb (output, result) -> "Result = " ++ show result ++
                             "; Output = " ++ output
-- `StateT` KEEPS OUTPUT, COUNTER AND DEBUG - MONADIC EVALUATOR USED JUST FOR
-- EXCEPTIONS

data MT a = FailT Exception
          | DoneT {unpackDoneT :: a}
            deriving Show

newtype StateTIO = StateTIO
    {
    unpackStateTIO :: (IOStack, Exception, Int)
    } deriving Show

mkMT :: a -> MT a
mkMT value = DoneT value

bindMT monad doNext = case monad of
  FailT err -> FailT err
  DoneT result -> doNext result

instance Monad MT where
  return = mkMT
  (>>=)  = bindMT

instance Functor MT where
  fmap _ (FailT err) = FailT err
  fmap f (DoneT result) = DoneT $ f result

-- The difference between `stopExecT_SIOE` and `catchT_SIOE` is that
-- `stopExecT_SIOE` just returns a failure, prompting the driving code to bail
-- on pattern matches. `catchT_SIOE` on the other hand produces a `DoneT`
-- instance, so the driving code is proceeding normally, only the error message
-- is extended.
stopExecT_SIOE :: Output -> StateT StateTIO MT Int
stopExecT_SIOE err = StateT (\_ -> FailT err)

catchT_SIOE :: Output -> StateT StateTIO MT ()
catchT_SIOE err = StateT (\(StateTIO (output, error, state)) ->
                           return (
                             (),
                             StateTIO (output, "Exception = " ++ err ++
                                       "; Iteration = " ++ show state, state)))
printT_SIOE :: Output -> StateT StateTIO MT ()
printT_SIOE out = StateT (\(StateTIO (outStack, error, state)) ->
                           return ((), StateTIO (out:outStack, error, state)))

incTStateIO :: StateT StateTIO MT ()
incTStateIO = StateT (\(StateTIO (outStack, error, state)) ->
                       return ((), StateTIO (outStack, error, state + 1)))
evaluateT_SIOE :: Int -> Output -> StateT StateTIO MT Int
evaluateT_SIOE result output = do
  incTStateIO
  printT_SIOE output
  case result of
    42 -> do
      catchT_SIOE "The ultimate answer. Goodbye!"
      return result
    11 -> stopExecT_SIOE "Number 11 is forbidden."
    otherwise -> return result
  
evalT_SIOE :: Term -> StateT StateTIO MT Int
evalT_SIOE con@(Con a) = evaluateT_SIOE a $ formatLine con a
evalT_SIOE add@(Add t u) = do
  a <- evalT_SIOE t
  b <- evalT_SIOE u
  let result = a + b
  evaluateT_SIOE result $ formatLine add result

runEvalRunT1 :: Term -> String
runEvalRunT1 exp = case runStateT
                           (evalT_SIOE exp)
                           (StateTIO ([], "", 0)) of
        FailT e -> e
        DoneT (result,StateTIO (outStack,error,state)) -> unwords
                [ "Result ="
                , show result
                , "; Iteration ="
                , show state
                , "; Output ="
                , show outStack
                , "; Errors = "
                , error]

runEvalRunT2 :: Term -> String
runEvalRunT2 exp = case runState
                           (StateTIO ([], "", 0))
                           (evalT_SIOE exp) of
        FailT e -> e
        DoneT (result,StateTIO (outStack,error,state)) -> unwords
                [ "Result ="
                , show result
                , "; Iteration ="
                , show state
                , "; Output ="
                , show outStack
                , "; Errors = "
                , error]

runEvalEvalT :: Term -> String
runEvalEvalT exp = case evalState
                           (StateTIO ([], "", 0))
                           (evalT_SIOE exp) of
        FailT e -> e
        DoneT result -> "Result = " ++ show result

runEvalExecT :: Term -> String
runEvalExecT exp = case execState
                           (StateTIO ([], "", 0))
                           (evalT_SIOE exp) of
        FailT e -> e
        DoneT (StateTIO (outStack, error, state)) -> unwords
                [ "; Iteration ="
                , show state
                , "; Output ="
                , show outStack
                , "; Errors = "
                , error]

main :: IO ()
main = do
    let term = Add (Con 5) (Con 6)
    let term42a = Add (Con 42) (Con 0)
    let term42b = Add (Con 0) (Con 42)
    let term42c = Add (Con 40) (Con 2)
    let evals = [ runEvalRunTa1, runEvalRunTa2, runEvalEvalTa, runEvalExecTa,
                  runEvalRunTb1, runEvalRunTb2, runEvalEvalTb, runEvalExecTb,
                  runEvalRunT1, runEvalRunT2, runEvalEvalT, runEvalExecT ]
    let terms = [term, term42a, term42b, term42c]
    mapM_ putStrLn $ evals <*> terms

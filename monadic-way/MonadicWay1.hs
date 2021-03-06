module MonadicWay.One where

import MonadicWay.Shared

eval :: Term -> Int
eval (Con a) = a
eval (Add a b) = eval a + eval b

type MOut a = (a, Output)

evalO :: Term -> MOut Int
evalO (Con a) = ( a
                , formatLine
                      (Con a)
                      a)
evalO (Add t u) = ( (a + b)
                  , (x ++
                     y ++
                     formatLine
                         (Add t u)
                         (a + b)))
    where (a,x) = evalO t
          (b,y) = evalO u
-- The core idea of binding is that e.g. in \x -> x + x and f 3 we bind x to 3.
-- (lambda calculus - bound variable)
-- The contextual operation performed by binding here is the concatenation of output.
bindM :: MOut a -> (a -> MOut b) -> MOut b
bindM m f = (b, x ++ y)
    where (a,x) = m
          (b,y) = f a
evalM_1 :: Term -> MOut Int
evalM_1 (Con a) = ( a
                  , formatLine
                        (Con a)
                        a)
evalM_1 add@(Add t u) = bindM
        (evalM_1 t)
        (\a    -- `a` is bound to `evalM_1 t`
           ->
              bindM
                  (evalM_1 u)
                  (\b    -- `b` is bound to `evalM_1 u`
                     ->
                        ( a + b
                        , formatLine
                              add
                              (a + b))))
-- identical to `bindM`, just so the next example is more readable
getIntFromType :: MOut a -> (a -> MOut b) -> MOut b
getIntFromType typeMOut doSomething = (newInt, oldOutput ++ newOutput)
    where (oldInt,oldOutput) = typeMOut
          (newInt,newOutput) = doSomething oldInt

evaluator :: Term -> MOut Int
evaluator (Con a) = (a, "output-")
evaluator (Add t u) = getIntFromType
        (evaluator t)
        (\firstInt ->
              getIntFromType
                  (evaluator u)
                  (\secondInt ->
                        (firstInt + secondInt, "-newoutput")))

evalM_2 :: Term -> MOut Int
evalM_2 (Con a) = (a, formatLine (Con a) a)
evalM_2 add@(Add t u) =
  evalM_2 t `bindM` \a ->
  evalM_2 u `bindM` \b ->
  (a+b, formatLine add (a+b))

mkM :: a -> MOut a
mkM a = (a, "")

outPut :: Output -> MOut ()
outPut x = ((), x)

evalM_3 :: Term -> MOut Int
evalM_3 (Con a) = outPut (formatLine (Con a) a) `bindM` \_ -> mkM a
evalM_3 (Add t u) = evalM_3 t `bindM` \a ->
  evalM_3 u `bindM` \b ->
    outPut (formatLine (Add t u) (a + b)) `bindM` \_ -> mkM (a+b)

-- convenience function for concatenating computations
-- without binding variables for later cases
combineM :: MOut a -> MOut b -> MOut b
combineM m f = m `bindM` \_ -> f

(>>) :: MOut a -> MOut b -> MOut b
m >> f = combineM m f

evalM :: Term -> MOut Int
evalM (Con a) = outPut (formatLine (Con a) a) `combineM` mkM a
evalM (Add t u) = evalM t `bindM` \a ->
  evalM u `bindM` \b ->
  outPut (formatLine (Add t u) (a + b)) `combineM` mkM (a+b)

--- FINAL VERSION

newtype Eval_IO a = Eval_IO (a, O) deriving (Show)

type Evaluator a = Term -> Eval_IO a

getInt :: Eval_IO a -> (a -> Eval_IO b) -> Eval_IO b
getInt monad doSomething = Eval_IO (newInt, oldOutput ++ newOutput)
  where Eval_IO (oldInt, oldOutput) = monad
        Eval_IO (newInt, newOutput) = doSomething oldInt

createEval_IO :: a -> Eval_IO a
createEval_IO int = Eval_IO (int, "")

print_IO :: O -> Eval_IO ()
print_IO string = Eval_IO ((), string)

evalM_4 :: Evaluator Int
evalM_4 (Con a) = createEval_IO a
evalM_4 (Add t u) = evalM_4 t `getInt` \a ->
  evalM_4 u `getInt` \b ->
  print_IO (formatLine (Add t u) (a + b)) `getInt` \_ -> createEval_IO (a + b)

{-
    The monad is defined by three elements:
    - the type, with it's type constructor
    - a `bind` method: binds an unwritten anonymous function's argument
      to the value of the type held within an `Eval_IO` instance. It also
      creates a series of anonymous functions - a line for each function.
    - a `return` function to put an `a` type value into an output `Eval_IO`

    Additionally, a `print_IO` is needed, which is inferred by Prelude:

    `p >> q = p >>= \_ -> q`
-}
instance Monad Eval_IO where
  return a = createEval_IO a
  (>>=) m f = getInt m f

-- By making `Eval_IO` a member of the `Monad` typeclass,
-- we can omit all the anonymous functions through `do`
-- notation sugar.
eval_IO :: Evaluator Int
eval_IO (Con a) = do
  print_IO (formatLine (Con a) a)
  return a
eval_IO (Add t u) = do                  -- the `do` notation sugar-coats the following:
  a <- eval_IO t                         -- `eval_IO t >>= \a -> ...`
  b <- eval_IO u                         -- `eval_IO u >>= \b -> ...`
  let result = a + b
  print_IO (formatLine (Add t u) result) -- `>>= \_ -> print_IO(...)`
  return result                          -- `>>= \result -> createEval_IO result`

-- VARIANT WITH ERROR HANDLING

data M a = Raise Exception
         | Return a
           deriving (Show)

evalE :: Term -> M Int
evalE (Con a) = Return a
evalE (Add a b) =
  case evalE a of
    Raise e -> Raise e
    Return a ->
      case evalE b of
        Raise e -> Raise e
        Return b ->
          if result == 42
             then Raise "The ultimate answer. Goodbye!"
             else Return result
             where result = a + b

-- Using a custom type for extracting all the pattern matching
-- out of the core logic.
data M1 a = Except Exception
          | Ok {showM :: a}
            deriving (Show)

instance Monad M1 where
  return = Ok
  m >>= f = case m of
    Except e -> Except e
    Ok a -> f a

raise :: Exception -> M1 a
raise = Except

eval_ME :: Term -> M1 Int
eval_ME (Con a) = return a  
eval_ME (Add t u) = do
    a <- eval_ME t
    b <- eval_ME u
    let result = a + b
    if result == 42
        then Except "The ultimate answer. Goodbye!"
        else Ok result

-- COMBINING THE OUTPUT-PRODUCING EVALUATOR WITH THE EXCEPTION-PRODUCING ONE
data M2 a = Ex Exception
          | Done {unpack :: (a, O)}
            deriving (Show)

instance Monad M2 where
  return a = Done (a, "")
  -- We are just offloading all of the concerns related to the computation
  -- into the monad definition. It seems to me as this is the case where monad
  -- stacks would be viable.
  m >>= f = case m of
    Ex e -> Ex e
    Done (a, x) -> case (f a) of
      Ex e1 -> Ex e1
      Done (b, y) -> Done (b, x++y)

-- Convenience method for raising an exception.
raise_IOE :: Exception -> M2 a
raise_IOE = Ex

-- Convenience method for putting a string into the data type instance.
print_IOE :: O -> M2 ()
print_IOE x = Done ((), x)

eval_IOE :: Term -> M2 Int
eval_IOE (Con a) = do
  print_IOE $ formatLine (Con a) a
  return a
eval_IOE (Add t u) = do
  a <- eval_IOE t
  b <- eval_IOE u
  let result = a + b -- We do not need to use `let ... in` because all bound
                     -- variables remain bound in the context of a `do`
                     -- procedure.
  let out = formatLine (Add t u) result
  print_IOE out
  if result == 42
     then raise_IOE $ out ++ "The ultimate answer. Goodbye!"
     else return result

-- STATEFUL VARIANT
-- non-monadic
type St a = State -> (a, State)
type State = Int

evalNMS :: Term -> St Int
-- Note that this is equivalent to `evalNMS :: Term -> State -> (Int, State)`.
-- Thus the function actually takes two arguments.
evalNMS (Con a) x = (a, x+1)
evalNMS (Add t u) x = let
  (a, y) = evalNMS t x
  (b, z) = evalNMS u y
  in (a + b, z + 1)

newtype MS a = MS { unpackMSandRun :: (State -> (a, State)) }

-- Creating the monad with an integer inside.
-- Takes an integer and applies the MS type constructor to an anonymous function that feeds the integer to a state.
mkMS :: a -> MS a
mkMS int = MS (\x -> (int, x))

bindMS :: MS a -> (a -> MS b) -> MS b
bindMS monad doNext = MS $ \initialState ->
  let (oldInt, oldState) = unpackMSandRun monad initialState
      (newInt, newState) = unpackMSandRun (doNext oldInt) oldState
  in  (newInt, newState)

instance Monad MS where
  return = mkMS
  m >>= f = bindMS m f

incState :: MS ()
incState = MS (\s -> ((), s+1))

evalMS :: Term -> MS Int
evalMS (Con a) = do incState -- increase the count
                    mkMS a   -- return `a`, given an initial state we get just
                    -- the value with an incremented counter.
evalMS (Add t u) = do
  a <- evalMS t 
  b <- evalMS u
  incState
  return (a + b)

evalMS2 :: Term -> MS Int
evalMS2 (Con a) = incState >>= (\_ -> mkMS a)
evalMS2 (Add t u) =
  evalMS t >>=
  (\a -> evalMS u >>=
         (\b -> incState >>= \_ -> return (a + b)))

-- MONADIC, STATEFUL VARIANT WITH OUTPUT

newtype Eval_SIO a = Eval_SIO { unpackMSIOandRun :: State -> (a, State, Output) }

bindMSIO :: Eval_SIO a -> (a -> Eval_SIO b) -> Eval_SIO b
bindMSIO monad doNext =
  Eval_SIO (\initialState ->
             let (oldInt, oldState, oldOutput) = unpackMSIOandRun monad initialState
                 (newInt, newState, newOutput) = unpackMSIOandRun (doNext oldInt) oldState
             in  (newInt, newState, oldOutput ++ newOutput))

mkMSIO :: a -> Eval_SIO a
mkMSIO int = Eval_SIO (\state -> (int, state, ""))

instance Monad Eval_SIO where
  return = mkMSIO
  m >>= f = bindMSIO m f

-- put a string inside an Eval_SIO instance.
print_SIO :: Output -> Eval_SIO ()
print_SIO output = Eval_SIO (\s -> ((), s, output))

incSIOState :: Eval_SIO ()
incSIOState = Eval_SIO (\state -> ((), state+1, ""))

eval_SIO :: Term -> Eval_SIO Int
eval_SIO con@(Con a) = do
  incSIOState
  print_SIO (formatLine con a)
  return a
eval_SIO add@(Add t u) = do
  a <- eval_SIO t
  b <- eval_SIO u
  let result = a + b
  incSIOState
  print_SIO (formatLine add result)
  return result

-- MONADIC, STATEFUL VARIANT WITH OUTPUT AND ERROR HANDLING

data EvaluationResult a =
  Good a State Output
  | Bad Exception State Output 
    deriving Show

newtype Eval_SIOE a = SIOE { runSIOE :: State -> EvaluationResult a }

raise_SIOE e = SIOE (\s -> Bad e s "")

print_SIOE :: String -> Eval_SIOE ()
print_SIOE out = SIOE (\s -> Good () s out)

incSIOEState :: Eval_SIOE()
incSIOEState = SIOE (\s -> Good () (s+1) "")

mkMSIOE :: a -> Eval_SIOE a
mkMSIOE int = SIOE (\s -> Good int s "")

bindMSIOE :: Eval_SIOE a -> (a -> Eval_SIOE b) -> Eval_SIOE b
bindMSIOE monad doNext = SIOE $ \initialState ->
  case runSIOE monad initialState of
    Good oldInt oldState oldOutput ->
      case runSIOE (doNext oldInt) oldState of
        Good newInt newState newOutput -> Good newInt newState (newOutput ++ oldOutput)
  -- reconstruction of the Bad instances is required for type conversion.
        Bad newError newState newOutput ->Bad newError newState (newOutput ++ oldOutput) 
    Bad oldError oldState oldOutput -> Bad oldError oldState oldOutput

instance Monad Eval_SIOE where
  return = mkMSIOE
  m >>= f = bindMSIOE m f

evalMSIOE :: Term -> Eval_SIOE Int 
evalMSIOE con@(Con a) = do
  incSIOEState
  let out = formatLine con a
  print_SIOE out
  if a == 42 then raise_SIOE $ out ++ "The ultimate answer. Goodbye!"
             else return a
evalMSIOE add@(Add t u) = do
  a <- evalMSIOE t
  b <- evalMSIOE u
  let result = a + b
  let out = formatLine add result
  incSIOEState
  if result == 42 then raise_SIOE $  out ++ "The ultimate answer. Goodbye!"
                  else return result

runEvalMSIOE :: Term -> String
runEvalMSIOE exp = case runSIOE (evalMSIOE exp) 0 of
  Bad error state output -> "Error: " ++ show error ++
                            " - at iteration " ++ show state ++
                            " - stack: " ++ show output
  good@(Good _ _ _) -> show good
  
main :: IO ()
main = do
    let term = Add (Con 5) (Con 6)
    let term42 = Add (Con 40) (Con 2)
    putStrLn $ show $ evalO term
    putStrLn $ show $ evaluator term
    putStrLn $ show $ evalM_2 term
    putStrLn $ show $ evalM term
    putStrLn $ show $ evalM_4 term
    putStrLn $ show $ eval_IO term
    putStrLn $ show $ eval_ME term
    putStrLn $ show $ eval_IOE term
    putStrLn $ show $ evalNMS term 0
    putStrLn $ show $ unpackMSandRun (evalMS term) 0
    putStrLn $ show $ unpackMSandRun (evalMS2 term) 0
    putStrLn $ show $ unpackMSIOandRun (eval_SIO term) 0
    putStrLn $ runEvalMSIOE term
    putStrLn $ runEvalMSIOE (Add (Con 42) (Con 0))
    putStrLn $ runEvalMSIOE (Add (Con 0) (Con 42))
    putStrLn $ runEvalMSIOE term42

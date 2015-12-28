module MonadicWay1 where
data Term
    = Con Int
    | Add Term
          Term
    deriving (Show)
eval :: Term -> Int
eval (Con a) = a
eval (Add a b) = eval a + eval b
type MOut a = (a, Output)
type Output = String

formatLine :: Term -> Int -> Output
formatLine t a = "eval (" ++ show t ++ ") <= " ++ show a ++ " - "

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
type O = String

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

main :: IO ()
main = do
    let term = Add (Con 5) (Con 6)
    putStrLn $ show $ evalO term
    putStrLn $ show $ evaluator term
    putStrLn $ show $ evalM_2 term
    putStrLn $ show $ evalM term
    putStrLn $ show $ evalM_4 term
    putStrLn $ show $ eval_IO term

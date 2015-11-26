{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import GradientDescent

-- stop condition respecting given error tolerance
stopCondition :: (Double -> Double)
                -> Double
                -> StopCondition (Double -> Double)
stopCondition f tolerance =
    let stop (Arg prev) (Arg cur) = abs (f prev - f cur) < tolerance
    in StopWhen stop

instance Floating a => GradientDescent (a -> a) where
    -- parameter for a function is its argument.
    data Params (a -> a) = Arg { unArg :: a  }

    -- numeric differentiation
    gradient f (Arg value) = Arg $ (f value - f (value - epsilon) / epsilon)
        where epsilon = 0.0001

    moveInParameterSpace scale (Arg vec) (Arg old) = 
        Arg $ old + fromRational (toRational scale) * vec

    gradientDescent function (StopWhen stop) alpha x0 =
        let iterations     = iterate takeStep x0
            iterationPairs = zip iterations $ tail iterations
         in -- Drop elements where parameters do not meet the stop condition.
            -- Return the last parameter set.
            -- uncurry :: (a -> b -> c) -> ((a, b) -> c)
            snd . head $ dropWhile (not . uncurry stop) iterationPairs
        where
            takeStep params = let gradients = gradient function params 
                               in moveInParameterSpace (-alpha) gradients params
main :: IO ()
main = do
  let function x = x^2 + 3*x
  let step = 1e-1
  let tolerance = 1e-4
  let initialValue = 12.0
  print $ "Calculating..."
  print $ unArg $ gradientDescent function (stopCondition function tolerance) step (Arg initialValue)

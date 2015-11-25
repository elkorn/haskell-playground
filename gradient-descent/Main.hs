{-# LANGUAGE FlexibleInstances #-}
module Main where

import GradientDescent

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
    -- stop condition respecting given error tolerance
    stopCondition :: (Double -> Double) 
                  -> Double 
                  -> StopCondition (Double -> Double)
    stopCondition f tolerance = 
        let stop (Arg prev) (Arg cur) = abs (f prev - f cur) < tolerance
        in StopWhen stop


main :: IO ()
main = print "Need to make this work."

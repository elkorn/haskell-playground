{-# LANGUAGE TypeFamilies #-}
module GradientDescent where

newtype StopCondition a = StopWhen (Params a -> Params a -> Bool)

class GradientDescent a where
    -- parameter space representation
    data Params a :: *
    -- leave the form of the stop condition to the implementer

    -- gradient at location a
    gradient :: a -> Params a -> Params a

    moveInParameterSpace :: Double   -- Scaling factor
                        -> Params a -- Direction vector
                        -> Params a -- Original location
                        -> Params a -- New location

    gradientDescent :: GradientDescent a => a -- what to optimize
                    -> StopCondition a        -- when to stop
                    -> Double                 -- step size
                    -> Params a               -- initial point
                    -> Params a               -- location of minimum



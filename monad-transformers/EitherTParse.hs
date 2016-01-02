{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EitherT.Parse where

import EitherT
import Control.Monad.State
import Data.Int (Int64)

import qualified Data.ByteString.Lazy as L

data ParseState = ParseState
    { string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

newtype Parse l a = P
    { runP :: EitherT l (State ParseState) a
    } deriving (Monad, MonadState ParseState)

evalParse :: Parse l a -> L.ByteString -> Either l a
evalParse m s = evalState (runEitherT $ runP m) $
  ParseState s 0

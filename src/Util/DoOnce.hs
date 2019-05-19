{-# LANGUAGE DeriveFunctor #-}

module Util.DoOnce where

import qualified Control.Monad as Monad

-- Do once is like the state monad, except it only keeps track of whether the
-- operation is the first one run in the monad or not
newtype DoOnce a = DoOnce { runDoOnce :: Bool -> a }
  deriving (Functor)

instance Applicative DoOnce where
  pure x = DoOnce (const x)
  (<*>) = Monad.ap

instance Monad DoOnce where
  (>>=) (DoOnce fn) k =
    DoOnce (\b0 -> (runDoOnce $ k $ fn b0) False)

evalDoOnce :: DoOnce a -> a
evalDoOnce (DoOnce f) = f True

isFirst :: DoOnce Bool
isFirst = DoOnce id

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Sharade.Pair (
  Pair(..), mPair
) where

  import Control.Monad
  import Control.Monad.Sharing

  data Pair m a b = Pair (m a) (m b)

  mPair :: (Sharing s) => s ((s a) -> s ((s b) -> s (Pair s a b)))
  mPair = return (\a -> return (\b -> return $ Pair a b))

  -- |
  -- This instance allows to use nested monadic pairs as argument to the
  -- 'Control.Monad.Sharing.share' combinator.
  instance (Monad m, Shareable m a, Shareable m b) => Shareable m (Pair m a b) where
    shareArgs f (Pair a b) = do
      a' <- f a
      b' <- f b
      return (Pair a' b')

  -- |
  -- This instance enables the function 'Control.Monad.Sharing.convert'
  -- to transform ordinary Haskell pairs into nested monadic pairs.
  instance (Monad m, Convertible m a a', Convertible m b b') => Convertible m (a, b) (Pair m a' b') where
    convert (a, b) = return (Pair (convert a) (convert b))

  -- |
  -- This instance enables the function 'Control.Monad.Sharing.convert'
  -- to transform nested monadic pairs into ordinary Haskell pairs.
  instance (Monad m, Convertible m a a', Convertible m b b') => Convertible m (Pair m a b) (a', b') where
    convert (Pair a b) = do
      a' <- a >>= convert
      b' <- b >>= convert
      return (a', b')
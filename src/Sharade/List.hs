{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Sharade.List (

  List(..), nil, cons, isEmpty, first, rest

 ) where

  import Control.Monad
  import Control.Monad.Sharing

  -- | Data type for lists where both the head and tail are monadic.
  data List s a = Nil | Cons (s a) (s (List s a))

  -- | The empty monadic list.
  nil :: Sharing s => s (List s a)
  nil = return Nil

  -- | Constructs a non-empty monadic list.
  cons :: Sharing s => s a -> s (List s a) -> s (List s a)
  cons x xs = return $ Cons x xs

  -- | Checks if monadic list is empty.
  isEmpty :: Sharing s => s (List s a) -> s Bool
  isEmpty xs = xs >>= (\xs -> case xs of
    Nil      -> return True
    Cons _ _ -> return False)

  -- |
  -- Yields the head of a monadic list. Relies on 'MonadPlus' instance
  -- to provide a failing implementation of 'fail'.
  first :: Sharing s => s (List s a) -> s a
  first xs = xs >>= (\xs -> case xs of
    Nil -> mzero
    Cons x _ -> x)

  -- |
  -- Yields the tail of a monadic list. Relies on 'MonadPlus' instance
  -- to provide a failing implementation of 'fail'.
  rest :: Sharing s => s (List s a) -> s (List s a)
  rest xs = xs >>= (\xs -> case xs of
    Nil -> mzero
    Cons _ xs -> xs)
  
  -- |
  -- This instance allows to use nested monadic lists as argument to the
  -- 'Control.Monad.Sharing.share' combinator.
  instance (Monad m, Shareable m a) => Shareable m (List m a) where
    shareArgs _ Nil         = return Nil
    shareArgs f (Cons x xs) = do
      y  <- f x
      ys <- f xs
      return (Cons y ys)

  -- |
  -- This instance enables the function 'Control.Monad.Sharing.convert'
  -- to transform ordinary Haskell lists into nested monadic lists.
  instance (Monad m, Convertible m a b) => Convertible m [a] (List m b) where
    convert []     = return Nil
    convert (x:xs) = return (Cons (convert x) (convert xs))

  -- |
  -- This instance enables the function 'Control.Monad.Sharing.convert'
  -- to transform nested monadic lists into ordinary Haskell lists.
  instance (Monad m, Convertible m a b) => Convertible m (List m a) [b] where
    convert Nil         = return []
    convert (Cons x xs) = do
      ys <- xs >>= convert
      y  <- x  >>= convert
      return (y:ys)

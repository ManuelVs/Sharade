{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Sharade.Prelude (
  Sharing(..), Shareable(..), Convertible(..), share, results, unsafeResults,
  mzero, mPlus,
  (<#>),
  mAdd, mSub, mMul, mDiv,
  mLt, mLeq, mGt, mGeq, mEq, mNeq,
  List(..), nil, cons, isEmpty, first, rest,
  Pair(..), mPair,
  true, false,
  mAnd, mOr
) where

  import Control.Monad
  import Control.Monad.Sharing
  
  import Sharade.List
  import Sharade.Pair

  mPlus :: (Sharing s) => s (s a -> s (s a -> s a))
  mPlus = return (\a -> return (\b -> mplus a b))

  infixl 4 <#>
  (<#>) :: (Sharing s) => s (s a -> s b) -> (s a) -> (s b)
  f <#> a = f >>= (\f' -> f' a)

  mAdd, mSub, mMul :: (Sharing s, Num a) => s (s a -> s (s a -> s a))
  mAdd = return (\a -> return (\b -> (return (+)) <*> a <*> b))
  mSub = return (\a -> return (\b -> (return (-)) <*> a <*> b))
  mMul = return (\a -> return (\b -> (return (*)) <*> a <*> b))

  mDiv :: (Sharing s, Fractional a) => s (s a -> s (s a -> s a))
  mDiv = return (\a -> return (\b -> (return (/)) <*> a <*> b))

  mLt, mLeq, mGt, mGeq, mEq, mNeq :: (Sharing s, Ord a) => s (s a -> s (s a -> s Bool))
  mLt  = return (\a -> return (\b -> return (<) <*> a <*> b))
  mLeq  = return (\a -> return (\b -> return (<=) <*> a <*> b))
  mGt  = return (\a -> return (\b -> return (>) <*> a <*> b))
  mGeq  = return (\a -> return (\b -> return (>=) <*> a <*> b))
  mEq  = return (\a -> return (\b -> return (==) <*> a <*> b))
  mNeq  = return (\a -> return (\b -> return (/=) <*> a <*> b))

  true, false :: (Sharing s) => s Bool
  true = return True
  false = return False

  mAnd, mOr :: (Sharing s) => s (s Bool -> s (s Bool -> s Bool))
  mAnd = return (\a -> return (\b -> (return (&&)) <*> a <*> b))
  mOr = return (\a -> return (\b -> (return (||)) <*> a <*> b))

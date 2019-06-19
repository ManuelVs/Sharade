{-# LANGUAGE
  MultiParamTypeClasses,
  FlexibleInstances
#-}

module Sharade.Prelude (
  Sharing(..), Shareable(..), Convertible(..), results, unsafeResults,
  mzero, mPlus,
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

  class Choiceable a where
    mPlus :: a -> a -> a
  
  instance {-# INCOHERENT #-} (Sharing s) => Choiceable (s a) where
    mPlus = mplus
  
  instance (Choiceable b) => Choiceable (a -> b) where
    f `mPlus` f' = (\x -> f x `mPlus` f' x)
  
  mAdd, mSub, mMul :: (MonadPlus s, Num a) => s a -> s a -> s a
  mAdd a b = a >>= (\a' -> b >>= (\b' -> return $ a' + b'))
  mSub a b = a >>= (\a' -> b >>= (\b' -> return $ a' - b'))
  mMul a b = a >>= (\a' -> b >>= (\b' -> return $ a' * b'))

  mDiv :: (MonadPlus s, Fractional a) => s a -> s a -> s a
  mDiv a b = a >>= (\a' -> b >>= (\b' -> return $ a' / b'))

  mLt, mLeq, mGt, mGeq, mEq, mNeq :: (Sharing s, Ord a) => s a -> s a -> s Bool
  mLt  a b = a >>= (\a' -> b >>= (\b' -> return $ a' < b'))
  mLeq a b = a >>= (\a' -> b >>= (\b' -> return $ a' <= b'))
  mGt  a b = a >>= (\a' -> b >>= (\b' -> return $ a' > b'))
  mGeq a b = a >>= (\a' -> b >>= (\b' -> return $ a' >= b'))
  mEq  a b = a >>= (\a' -> b >>= (\b' -> return $ a' == b'))
  mNeq a b = a >>= (\a' -> b >>= (\b' -> return $ a' /= b'))

  true, false :: (Sharing s) => s Bool
  true  = return True
  false = return False

  mAnd, mOr :: (Sharing s) => s Bool -> s Bool -> s Bool
  mAnd a b = a >>= (\a' -> if a' then b else false)
  mOr  a b = a >>= (\a' -> if a' then true else b)

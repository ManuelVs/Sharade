module Sharade.Prelude (
  mplus, Sharing(), share, results, mAdd, mSub, mMul, mDiv, mLt, mLeq, mGt, mGeq, mEq, mNeq
) where

  import Control.Monad
  import Control.Monad.Sharing

  mAdd, mSub, mMul :: (Sharing s, Num a) => s a -> s a -> s a
  mAdd = (\p1 p2 -> return (+) <*> p1 <*> p2)
  mSub = (\p1 p2 -> return (-) <*> p1 <*> p2)
  mMul = (\p1 p2 -> return (*) <*> p1 <*> p2)

  mDiv :: (Sharing s, Fractional a) => s a -> s a -> s a
  mDiv = (\p1 p2 -> return (/) <*> p1 <*> p2)

  mLt, mLeq, mGt, mGeq, mEq, mNeq :: (Sharing s, Ord a) => s a -> s a -> s Bool
  mLt  = (\p1 p2 -> return (<) <*> p1 <*> p2)
  mLeq = (\p1 p2 -> return (<=) <*> p1 <*> p2)
  mGt  = (\p1 p2 -> return (>) <*> p1 <*> p2)
  mGeq = (\p1 p2 -> return (>=) <*> p1 <*> p2)
  mEq  = (\p1 p2 -> return (==) <*> p1 <*> p2)
  mNeq  = (\p1 p2 -> return (/=) <*> p1 <*> p2)

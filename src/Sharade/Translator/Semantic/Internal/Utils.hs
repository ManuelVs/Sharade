module Sharade.Translator.Semantic.Internal.Utils where

  import Data.List

  allUnique :: (Ord a) => [a] -> Bool
  allUnique = allUnique' . sort where
    allUnique' [] = True
    allUnique' [_] = True
    allUnique' (x:y:xs)
      | x == y = False
      | otherwise = allUnique' (y:xs)
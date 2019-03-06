{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sharade.Translator.Semantic.TypeEnv where
  import Data.Semigroup

  import qualified Data.Map as Map

  import Sharade.Translator.Semantic.Type
  import Sharade.Parser.Syntax

  newtype TypeEnv = TypeEnv (Map.Map VarName Scheme) deriving (Semigroup, Monoid)

  extend :: TypeEnv -> (VarName, Scheme) -> TypeEnv
  extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

  emptyTyenv :: TypeEnv
  emptyTyenv = TypeEnv Map.empty

  typeof :: TypeEnv -> VarName -> Maybe Scheme
  typeof (TypeEnv env) name = Map.lookup name env
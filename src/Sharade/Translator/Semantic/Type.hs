module Sharade.Translator.Semantic.Type where

  type TVarName = String -- Like VarName
  type TName = String    -- Type name

  newtype TVar = TV TVarName deriving (Show, Eq, Ord)

  data Type
    = TVar TVar
    | TCon TName
    | TArr Type Type
    deriving (Show, Eq, Ord)

  data Scheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)
  
  boolType, numberType :: Type
  boolType = TCon "Bool"
  numberType = TCon "Number"
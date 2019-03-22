module Sharade.Translator.Semantic.Type where

  type TVarName = String -- Like VarName
  type TName = String    -- Type name
  type CName = String    -- Class name

  newtype TVar = TV TVarName deriving (Show, Eq, Ord)

  data Type
    = TVar TVar
    | TCon TName
    | TList Type
    | TPair Type Type
    | TArr Type Type
    deriving (Show, Eq, Ord)

  data Scheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)

  boolType, integerType, doubleType, charType :: Type
  boolType = TCon "Bool"
  integerType = TCon "Integer"
  doubleType = TCon "Double"
  charType = TCon "Char"
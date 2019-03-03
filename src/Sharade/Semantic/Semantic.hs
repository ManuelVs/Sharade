module Sharade.Semantic.Semantic where

  -- Expressions with type information
  data TExpr =
    TLit Type String |
    TVar Type String |
    TBind Type TBinding TExpr |
    TFun Type TExpr TExpr
    deriving Show

  data TBinding = TB String TExpr deriving Show

  data TFDecl = TFDecl Type String [String] TExpr deriving Show

  data Type =
    Any          |  -- Any type
    SType String |  -- Simple type. "Int"
    LType Type   |  -- List type
    FType Type Type -- Function type. "Int -> Int -> [Int]"
    deriving (Eq, Show)
  
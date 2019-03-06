module Sharade.Parser.Syntax where
  
  type VarName = String -- Represents a variable. Should be a lowercase starting identifier
  type LitName = String

  data Expr =
    Lit LitName |          -- Literals. Numbers, identifiers starting with uppercase (True, False).
    Var VarName |          -- Variables, identifiers starting with lowercase.
    Ch VarName Expr Expr | -- Choose expression. choose a = 2 ? 3 in expr
    Let VarName Expr Expr| -- Let expression. let a = 2 ? 3 in expr
    Lam VarName Expr     | -- Lamda expression
    App Expr Expr          -- Currified function. (f 5 8 -> (App (App f 5) (Lit 8)))
    deriving (Show)

  type FDecl = (VarName, Expr)
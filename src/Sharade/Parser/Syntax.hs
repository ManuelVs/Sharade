module Sharade.Parser.Syntax where
  
  type Var = String -- Represents a variable. Should be a lowercase starting identifier

  data Expr =
    Lit String |       -- Literals. Numbers, identifiers starting with uppercase (True, False).
    Var Var |          -- Variables, identifiers starting with lowercase.
    Ch Var Expr Expr | -- Choose expression. choose a = 2 ? 3 in expr
    Let Var Expr Expr| -- Let expression. let a = 2 ? 3 in expr
    Lam Var Expr     | -- Lamda expression
    App Expr Expr      -- Currified function. (f 5 8 -> (App (App f 5) (Lit 8)))
    deriving (Show)

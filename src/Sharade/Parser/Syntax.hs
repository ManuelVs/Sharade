module Sharade.Parser.Syntax where
  
  type Var = String

  data SExpr =
      Lit String          -- Literals. Numbers, identifiers starting with uppercase (True, False).
    | Var String          -- Variables, identifiers starting with lowercase.
    | Bind SBinding SExpr -- Bind expression. choose a = 2 ? 3 in expr
    | Fun SExpr SExpr     -- Currified function. (f 5 8 -> (Fun (Fun f 5) (Lit 8)))
    deriving (Show)
    
  -- Binding. For example, in 'choose' clausule
  data SBinding = B String SExpr deriving (Show)
  
  data FDecl = FDecl String [String] SExpr deriving (Show)
  
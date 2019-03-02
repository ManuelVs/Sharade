module Sharade.Parser.Syntax where
  
  type Var = String

  data Expr =
      HDf String        -- Haskell definitions like infixed operators.
    | Lit String        -- Literals.
    | MDf String        -- Any other definition.
    | Bind Binding Expr -- Bind expression. choose a = 2 ? 3 in ...
    | Ch Expr Expr      -- Choice between two expressions
    | AFun Expr Expr    -- Applicative function. (f 5 8 -> (AFun (AFun f 5) (Lit 8)))
    deriving (Show)
    
  -- Binding. For example, in 'choose' clausule
  data Binding = B Var Expr deriving (Show)
  
  data Decl = SDecl Var [Var] Expr deriving (Show)
  
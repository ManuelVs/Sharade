module Sharade.Translator.Eval (
  right, translateDecl
) where

  import Sharade.Parser.Syntax

  right :: Either a b -> b
  right (Right b) = b
  right _ = error "Not a right expression"

  translateExpr :: Expr -> String
  translateExpr (AFun le re) =
    translateExpr le ++
    " (" ++ translateExpr re ++ ")"
  
  translateExpr (Ch le re) = "mplus " ++
    "(" ++ translateExpr le ++ ") " ++
    "(" ++ translateExpr re ++ ")"
  
  translateExpr (HDf s) = "(\\p1 p2 -> (return " ++ s ++ ") <*> p1 <*> p2)"
  translateExpr (Lit l) = "return " ++ l
  translateExpr (MDf l) = l
  translateExpr (Bind b e) = translateBind b e

  translateBind :: Binding -> Expr -> String
  translateBind (B v e) se =
    "share (" ++ translateExpr e ++ ") >>= (\\" ++ v ++ " -> " ++
    translateExpr se ++ 
    ")"

  translateDecl :: Decl -> String
  translateDecl (SDecl fun args expr) =
    fun ++
    concatMap (\x -> " " ++ x) args ++ 
    " = " ++
    translateExpr expr

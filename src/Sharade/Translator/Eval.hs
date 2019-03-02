module Sharade.Translator.Eval (
  right, translateDecl
) where

  import Sharade.Parser.Syntax

  right :: Either a b -> b
  right (Right b) = b
  right _ = error "Not a right expression"

  translateExpr :: SExpr -> String
  translateExpr (AFun le re) =
    translateExpr le ++
    " (" ++ translateExpr re ++ ")"
  
  translateExpr (Var s) = "(\\p1 p2 -> (return " ++ s ++ ") <*> p1 <*> p2)"
  translateExpr (Lit l) = "return " ++ l
  translateExpr (Bind b e) = translateBind b e

  translateBind :: SBinding -> SExpr -> String
  translateBind (B v e) se =
    "share (" ++ translateExpr e ++ ") >>= (\\" ++ v ++ " -> " ++
    translateExpr se ++ 
    ")"

  translateDecl :: FDecl -> String
  translateDecl (FDecl fun args expr) =
    fun ++
    concatMap (\x -> " " ++ x) args ++ 
    " = " ++
    translateExpr expr

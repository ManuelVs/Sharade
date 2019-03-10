module Sharade.Translator.Translator (
  translateModule
) where
  
  import Data.Foldable
  import qualified Data.Map as Map

  import Sharade.Translator.Semantic.Type
  import Sharade.Translator.Semantic.TypeEnv
  import Sharade.Translator.Semantic.TypesEvaluation
  import Sharade.Parser.Syntax

  translateVarExpr :: VarName -> String
  translateVarExpr "(+)" = "mAdd"
  translateVarExpr "(-)" = "mSub"
  translateVarExpr "(*)" = "mMul"
  translateVarExpr "(/)" = "mDiv"
  translateVarExpr "(<)" = "mLt"
  translateVarExpr "(<=)" = "mLeq"
  translateVarExpr "(>)" = "mGt"
  translateVarExpr "(>=)" = "mGeq"
  translateVarExpr "(==)" = "mEq"
  translateVarExpr "(!=)" = "mNeq"
  translateVarExpr "(?)" = "mPlus"
  translateVarExpr v = v

  translateExpr :: Expr -> String
  translateExpr (Lit l) = "return " ++ l

  translateExpr (Var v) = translateVarExpr v

  translateExpr (Ch v b e) =
    "share (" ++ translateExpr b ++ ") >>= (\\" ++ v ++ " -> " ++
    translateExpr e ++ 
    ")"
  
  translateExpr (Let v b e) =
    "let " ++ v ++ " = " ++ translateExpr b ++ " in " ++ translateExpr e

  translateExpr (Lam x e) =
    "return (\\" ++ x ++ " -> " ++ translateExpr e ++ ")"

  translateExpr (Fix (Lam _ e)) =
    --"mFix (" ++ translateExpr e ++ ")"
    translateExpr e

  translateExpr (Case e ms) =
    "(" ++ translateExpr e ++ ") >>= (\\pec -> case pec of {" ++
    concatMap translateMatch ms ++ "})"

  translateExpr (App le re) =
    translateExpr le ++ " <#> (" ++ translateExpr re ++ ")"
  
  translateMatch :: Match -> String
  translateMatch (Match (PLit l) e) = l ++ " -> " ++ translateExpr e ++ ";"
  translateMatch (Match (PVar v) e) =
    v ++ " -> (\\" ++ v ++ " -> " ++ translateExpr e ++ ") (return " ++ v ++ ");"
  
  translateType :: Type -> String
  translateType (TVar (TV v)) = "s " ++ v
  translateType (TCon "Number") = "s Double"
  translateType (TCon v) = "s " ++ v
  translateType (TArr lt rt) = "s ((" ++ translateType lt ++ ") -> " ++ translateType rt ++ ")"
  

  translateDecl :: FDecl -> Type -> Either TypeError String
  translateDecl (funName, expr) t = do
    let tStr = funName ++ " :: (Sharing s) => " ++ translateType t
    let eStr = translateExpr expr
    return $ tStr ++ "\n" ++ funName ++ " = " ++ eStr

  lookupTypeEnv :: TypeEnv -> VarName -> Either TypeError Scheme
  lookupTypeEnv (TypeEnv env) x = case Map.lookup x env of
    Nothing -> Left $ UnboundVariable x
    Just t -> Right t


  translateModule :: [FDecl] -> Either TypeError String
  translateModule md = do
    ts <- inferTop preludeTypeEnv md
    foldrM (\f@(funName, _) s -> do
      Forall _ t <- lookupTypeEnv ts funName
      tf <- translateDecl f t
      return $ tf ++ "\n" ++ s) "" md
  
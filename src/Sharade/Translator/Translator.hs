module Sharade.Translator.Translator (
  translateModule
) where
  
  import Data.Foldable
  import qualified Data.Map as Map

  import Sharade.Translator.Semantic.Type
  import Sharade.Translator.Semantic.TypeEnv
  import Sharade.Translator.Semantic.TypesEvaluation
  import Sharade.Parser.Syntax

  reservedVarTranslation :: VarName -> Maybe String
  reservedVarTranslation "(+)" = Just "(\\p1 p2 -> (return (+)) <*> p1 <*> p2)"
  reservedVarTranslation "(?)" = Just "(\\p1 p2 -> mplus p1 p2)"
  reservedVarTranslation _ = Nothing

  translateExpr :: Expr -> String
  translateExpr (Lit l) = "return " ++ l

  translateExpr (Var v) = translateExpr' (reservedVarTranslation v) where
    translateExpr' (Just v) = v
    translateExpr' Nothing = v

  translateExpr (Ch v b e) =
    "share (" ++ translateExpr b ++ ") >>= (\\" ++ v ++ " -> " ++
    translateExpr e ++ 
    ")"
  
  translateExpr (Let v b e) =
    "let " ++ v ++ " = " ++ translateExpr b ++ " in " ++ translateExpr e

  translateExpr (Lam x e) =
    "(\\" ++ x ++ " -> " ++ translateExpr e ++ ")"

  translateExpr (App le re) =
    translateExpr le ++ " (" ++ translateExpr re ++ ")"

  
  translateType :: Type -> String
  translateType (TVar (TV v)) = "s " ++ v
  translateType (TCon "Number") = "s Double"
  translateType (TCon v) = "s " ++ v
  translateType (TArr lt rt) = "(" ++ translateType lt ++ ") -> " ++ translateType rt
  

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


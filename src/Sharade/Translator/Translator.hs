module Sharade.Translator.Translator (
  translateModule
) where
  
  import Data.List
  import Data.Foldable
  import qualified Data.Map as Map

  import Sharade.Translator.Semantic.Type
  import Sharade.Translator.Semantic.TypeEnv
  import Sharade.Translator.Semantic.Infer
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
  translateVarExpr "Nil" = "nil"
  translateVarExpr "Cons" = "cons"
  translateVarExpr v = v

  genericTVar :: Type
  genericTVar = TVar $ TV "a"

  preludeTypeEnv :: TypeEnv
  preludeTypeEnv = TypeEnv $ Map.fromList [
    ("(?)", Forall [TV "a"] $ TArr genericTVar (TArr genericTVar genericTVar)),
    ("(+)", Forall [] $ TArr numberType (TArr numberType numberType)),
    ("(-)", Forall [] $ TArr numberType (TArr numberType numberType)),
    ("(*)", Forall [] $ TArr numberType (TArr numberType numberType)),
    ("(/)", Forall [] $ TArr numberType (TArr numberType numberType)),
    ("(>)", Forall [] $ TArr numberType (TArr numberType boolType)),
    ("(>=)", Forall [] $ TArr numberType (TArr numberType boolType)),
    ("(<)", Forall [] $ TArr numberType (TArr numberType boolType)),
    ("(<=)", Forall [] $ TArr numberType (TArr numberType boolType)),
    ("(==)", Forall [] $ TArr numberType (TArr numberType boolType)),
    ("(!=)", Forall [] $ TArr numberType (TArr numberType boolType)),
    ("Nil", Forall [TV "a"] $ TList genericTVar),
    ("Cons", Forall [TV "a"] $ TArr genericTVar (TArr (TList genericTVar) (TList genericTVar)))
    ]
  
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

  translateExpr (Fix (Lam _ e)) = translateExpr e

  translateExpr (Case e ms) =
    "(" ++ translateExpr e ++ ") >>= (\\pec -> case pec of {" ++
    concatMap translateMatch ms ++ 
    "_ -> mzero;})"

  translateExpr (App le re) =
    translateExpr le ++ " <#> (" ++ translateExpr re ++ ")"
  
  translateMatch :: Match -> String
  translateMatch (Match (PVar v) e) =
    v ++ " -> (\\" ++ v ++ " -> " ++ translateExpr e ++ ") (return " ++ v ++ ");"
  translateMatch (Match p e) = translatePattern p ++ " -> " ++ translateExpr e ++ ";"

  translatePattern :: Pattern -> String
  translatePattern (PLit l) = l
  translatePattern (PVar v) = v
  translatePattern (PCon c ps) =
    "(" ++  intercalate " " (c : map translatePattern ps) ++ ")"
  
  translateType :: Type -> String
  translateType (TVar (TV v)) = "s " ++ v
  translateType (TCon "Number") = "s Double"
  translateType (TCon v) = "s " ++ v
  translateType (TList t) = "s (List s (" ++ translateListedType t ++ "))" where
    translateListedType (TVar (TV v)) = v
    translateListedType (TCon "Number") = "Double"
    translateListedType (TCon v) = v
    translateListedType (TList t) = "(List s (" ++ translateListedType t ++ "))"
    translateListedType (TArr lt rt) = "((" ++ translateType lt ++ ") -> " ++ translateType rt ++ ")"
  translateType (TArr lt rt) = "s ((" ++ translateType lt ++ ") -> " ++ translateType rt ++ ")"

  lookupTypeEnv :: TypeEnv -> VarName -> Either TypeError Scheme
  lookupTypeEnv (TypeEnv env) x = case Map.lookup x env of
    Nothing -> Left $ UnboundVariable x
    Just t -> Right t

  translateModule :: [FDecl] -> Either TypeError String
  translateModule md = do
    ts <- inferTop preludeTypeEnv md
    foldrM (\(funName, e) s -> do
      Forall _ t <- lookupTypeEnv ts funName
      let tStr = funName ++ " :: (Sharing s) => " ++ translateType t
      let eStr = funName ++ " = " ++ translateExpr e
      return $ tStr ++ "\n" ++ eStr ++ "\n" ++ s) "" md

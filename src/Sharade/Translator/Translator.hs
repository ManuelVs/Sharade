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
  translateVarExpr "(?)" = "mPlus"
  translateVarExpr "Nil" = "nil"
  translateVarExpr "Cons" = "cons"
  translateVarExpr "True" = "true"
  translateVarExpr "False" = "false"
  translateVarExpr "Pair" = "mPair"

  translateVarExpr "(&&)" = "mAnd"
  translateVarExpr "(||)" = "mOr"

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

  translateVarExpr "dAdd" = "mAdd"
  translateVarExpr "dSub" = "mSub"
  translateVarExpr "dMul" = "mMul"
  translateVarExpr "dDiv" = "mDiv"
  translateVarExpr "dLt" = "mLt"
  translateVarExpr "dLeq" = "mLeq"
  translateVarExpr "dGt" = "mGt"
  translateVarExpr "dGeq" = "mGeq"
  translateVarExpr "dEq" = "mEq"
  translateVarExpr "dNeq" = "mNeq"

  translateVarExpr v = v

  genericTVar :: Type
  genericTVar = TVar $ TV "a"

  preludeTypeEnv :: TypeEnv
  preludeTypeEnv = TypeEnv $ Map.fromList [
    ("(?)", Forall [TV "a"] $ TArr genericTVar (TArr genericTVar genericTVar)),
    ("Nil", Forall [TV "a"] $ TList genericTVar),
    ("Cons", Forall [TV "a"] $ TArr genericTVar (TArr (TList genericTVar) (TList genericTVar))),
    ("True", Forall [] boolType),
    ("False", Forall [] boolType),
    ("Pair", Forall [TV "a", TV "b"] $ TArr (TVar $ TV "a") (TArr (TVar $ TV "b") (TPair (TVar $ TV "a") (TVar $ TV "b")))),

    ("(&&)", Forall [] $ TArr boolType (TArr boolType boolType)),
    ("(||)", Forall [] $ TArr boolType (TArr boolType boolType)),

    ("(+)", Forall [] $ TArr integerType (TArr integerType integerType)),
    ("(-)", Forall [] $ TArr integerType (TArr integerType integerType)),
    ("(*)", Forall [] $ TArr integerType (TArr integerType integerType)),
    ("(/)", Forall [] $ TArr integerType (TArr integerType integerType)),
    ("(>)", Forall [] $ TArr integerType (TArr integerType boolType)),
    ("(>=)", Forall [] $ TArr integerType (TArr integerType boolType)),
    ("(<)", Forall [] $ TArr integerType (TArr integerType boolType)),
    ("(<=)", Forall [] $ TArr integerType (TArr integerType boolType)),
    ("(==)", Forall [] $ TArr integerType (TArr integerType boolType)),
    ("(!=)", Forall [] $ TArr integerType (TArr integerType boolType)),

    ("dAdd", Forall [] $ TArr doubleType (TArr doubleType doubleType)),
    ("dSub", Forall [] $ TArr doubleType (TArr doubleType doubleType)),
    ("dMul", Forall [] $ TArr doubleType (TArr doubleType doubleType)),
    ("dDiv", Forall [] $ TArr doubleType (TArr doubleType doubleType)),
    ("dLt" , Forall [] $ TArr doubleType (TArr doubleType boolType)),
    ("dLeq", Forall [] $ TArr doubleType (TArr doubleType boolType)),
    ("dGt" , Forall [] $ TArr doubleType (TArr doubleType boolType)),
    ("dGeq", Forall [] $ TArr doubleType (TArr doubleType boolType)),
    ("dEq", Forall [] $ TArr doubleType (TArr doubleType boolType)),
    ("dNeq", Forall [] $ TArr doubleType (TArr doubleType boolType))
    ]
  
  translateExpr :: Expr -> String
  translateExpr (Lit (IValue i)) = "return " ++ show i
  translateExpr (Lit (DValue d)) = "return " ++ show d
  translateExpr (Lit (CValue c)) = "return " ++ show c

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
  translateMatch (Match (PVar v) e)
    | v `isIn` e =
      v ++ " -> (\\" ++
      v ++ " -> " ++ translateExpr e ++
      ") (return " ++ v ++ ");"
    | otherwise = v ++ " -> " ++ translateExpr e ++ ";"
    where
      isIn v (Lit _) = False
      isIn v (Var v') = v == v'
      isIn v (Ch v1 e1 e2)
        | isIn v e1 = True
        | v == v1 = False
        | isIn v e2 = True
        | otherwise = False
      isIn v (Let v1 e1 e2)
        | isIn v e1 = True
        | v == v1 = False
        | isIn v e2 = True
        | otherwise = False
      isIn v (Lam v' e) = v /= v' && isIn v e
      isIn v (Fix e) = isIn v e
      isIn v (App e1 e2) = isIn v e1 || isIn v e2
      isIn v (Case e ms) = isIn v e || any (isInMatch v) ms

      isInMatch v (Match p e) = not (isInPattern v p) && isIn v e
      
      isInPattern v (PVar v') = v == v'
      isInPattern v (PLit _) = False
      isInPattern v (PCon _ ps) = any (isInPattern v) ps

  translateMatch (Match p e) = translatePattern p ++ " -> " ++ translateExpr e ++ ";"

  translatePattern :: Pattern -> String
  translatePattern (PLit (IValue i)) = show i
  translatePattern (PLit (DValue d)) = show d
  translatePattern (PLit (CValue c)) = show c
  translatePattern (PVar v) = v
  translatePattern (PCon c ps) =
    "(" ++  intercalate " " (c : map translatePattern ps) ++ ")"
  
  translateType :: Type -> String
  translateType (TVar (TV v)) = "s " ++ v
  translateType (TCon "Number") = "s Double"
  translateType (TCon v) = "s " ++ v
  translateType (TArr lt rt) = "s ((" ++ translateType lt ++ ") -> " ++ translateType rt ++ ")"
  translateType (TList t) = "s (List s (" ++ translateNoMonadType t ++ "))"
  translateType (TPair lt rt) = "s (Pair s (" ++ translateNoMonadType lt ++ ") (" ++ translateNoMonadType rt ++ "))"

  translateNoMonadType (TVar (TV v)) = v
  translateNoMonadType (TCon "Number") = "Double"
  translateNoMonadType (TCon v) = v
  translateNoMonadType (TArr lt rt) = "((" ++ translateType lt ++ ") -> " ++ translateType rt ++ ")"
  translateNoMonadType (TList t) = "(List s (" ++ translateNoMonadType t ++ "))"
  translateNoMonadType (TPair lt rt) = "(Pair s (" ++ translateNoMonadType lt ++ ") (" ++ translateNoMonadType rt ++ "))"

  lookupTypeEnv :: TypeEnv -> VarName -> Either TypeError Scheme
  lookupTypeEnv (TypeEnv env) x = case Map.lookup x env of
    Nothing -> Left $ UnboundVariable x
    Just t -> Right t

  translateModule :: [FDecl] -> Either TypeError String
  translateModule md = do
    ts <- inferTop preludeTypeEnv md
    let mStr = "import Sharade.Prelude"
    mDef <- foldrM (\(funName, e) s -> do
      Forall _ t <- lookupTypeEnv ts funName
      let tStr = funName ++ " :: (Sharing s) => " ++ translateType t
      let eStr = funName ++ " = " ++ translateExpr e
      return $ tStr ++ "\n" ++ eStr ++ "\n" ++ s) "" md
    return $ mStr ++ "\n" ++ mDef

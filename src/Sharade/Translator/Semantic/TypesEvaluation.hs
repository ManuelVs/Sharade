{- Mostly the same as https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter7/poly/src/Infer.hs
-- That project is licensed under the MIT license.
-}

module Sharade.Translator.Semantic.TypesEvaluation where

  import Control.Monad.State
  import Control.Monad.Except

  import Data.List (nub)
  import Data.Foldable (foldr)
  import qualified Data.Map as Map
  import qualified Data.Set as Set

  import Sharade.Parser.Syntax
  import Sharade.Translator.Semantic.Type
  import Sharade.Translator.Semantic.Substitutable
  import Sharade.Translator.Semantic.TypeEnv

  data Unique = Unique { count :: Int }
  initUnique :: Unique
  initUnique = Unique { count = 0 }
  
  data TypeError
    = UnificationFail Type Type
    | InfiniteType TVar Type
    | UnboundVariable VarName deriving Show
  type Infer = ExceptT TypeError (State Unique)

  normalize :: Scheme -> Scheme
  normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
    where
      ord = zip (nub $ fv body) (fmap TV letters)

      fv (TVar a)   = [a]
      fv (TArr a b) = fv a ++ fv b
      fv (TCon _)   = []

      normtype (TArr a b) = TArr (normtype a) (normtype b)
      normtype (TCon a)   = TCon a
      normtype (TVar a)   =
        case lookup a ord of
          Just x -> TVar x
          Nothing -> error "type variable not in signature"
  
  lookupEnv :: TypeEnv -> VarName -> Infer (Subst, Type)
  lookupEnv (TypeEnv env) x =
    case Map.lookup x env of
      Nothing -> throwError $ UnboundVariable (show x)
      Just s  -> do t <- instantiate s
                    return (nullSubst, t)

  unify ::  Type -> Type -> Infer Subst
  unify (l `TArr` r) (l' `TArr` r')  = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return (s2 `compose` s1)

  unify (TVar a) t = bind a t
  unify t (TVar a) = bind a t
  unify (TCon a) (TCon b) | a == b = return nullSubst
  unify t1 t2 = throwError $ UnificationFail t1 t2

  bind ::  TVar -> Type -> Infer Subst
  bind a t
    | t == TVar a     = return nullSubst
    | occursCheck a t = throwError $ InfiniteType a t
    | otherwise       = return $ Map.singleton a t

  occursCheck ::  Substitutable a => TVar -> a -> Bool
  occursCheck a t = a `Set.member` ftv t

  letters :: [TVarName]
  letters = [1..] >>= flip replicateM ['a'..'z']

  fresh :: Infer Type
  fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

  instantiate ::  Scheme -> Infer Type
  instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Map.fromList $ zip as as'
    return $ apply s t

  generalize :: TypeEnv -> Type -> Scheme
  generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

  typeOfLiteral :: String -> Type
  typeOfLiteral "True" = boolType
  typeOfLiteral "False" = boolType
  typeOfLiteral _ = numberType
  
  infer :: TypeEnv -> Expr -> Infer (Subst, Type)
  infer _ (Lit l) = return (nullSubst, typeOfLiteral l)
  infer env (Var x) = lookupEnv env x
  
  -- Choose expression.
  infer env (Ch x e1 e2) = do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t'   = generalize env' t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s2 `compose` s1, t2)
  
  -- Let expression.
  infer env (Let x e1 e2) = do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t'   = generalize env' t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s2 `compose` s1, t2)
  
  --  Lamda
  infer env (Lam x e) = do
    tv <- fresh
    let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArr` t1)
  
  -- Function application
  infer env (App e1 e2) = do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3       <- unify (apply s2 t1) (TArr t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)


  inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
  inferPrim env l t = do
    tv <- fresh
    (s1, tf) <- foldM inferStep (nullSubst, id) l
    s2 <- unify (apply s1 (tf tv)) t
    return (s2 `compose` s1, apply s2 tv)
    where
    inferStep (s, tf) exp = do
      (s', t) <- infer (apply s env) exp
      return (s' `compose` s, tf . (TArr t))


  runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
  runInfer m = case evalState (runExceptT m) initUnique of
    Left err  -> Left err
    Right res -> Right $ closeOver res

  closeOver :: (Map.Map TVar Type, Type) -> Scheme
  closeOver (sub, ty) = normalize sc
    where sc = generalize emptyTyenv (apply sub ty)

  
  inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
  inferExpr env = runInfer . infer env

  inferTop :: TypeEnv -> [(VarName, Expr)] -> Either TypeError TypeEnv
  inferTop env [] = Right env
  inferTop env ((name, ex):xs) = case inferExpr env ex of
    Left err -> Left err
    Right ty -> inferTop (extend env (name, ty)) xs

  
  
  genericTVar :: Type
  genericTVar = TVar $ TV "a"

  preludeTypeEnv :: TypeEnv
  preludeTypeEnv = TypeEnv $ Map.fromList [
    ("(?)", Forall [] $ TArr numberType (TArr numberType numberType)),
    ("(+)", Forall [TV "a"] $ TArr numberType (TArr numberType numberType)) ]
    --("(-)",  TArr numberType (TArr numberType numberType))),
    --("(*)",  TArr numberType (TArr numberType numberType))),
    --("(/)",  TArr numberType (TArr numberType numberType))),
  --]
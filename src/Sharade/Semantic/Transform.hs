module Sharing.Semantic.Transform where

  import Control.Monad
  import qualified Data.Map as Map
  
  import Debug.Trace

  import Sharade.Parser.Syntax
  import Sharade.Semantic.Semantic
  
  type Scope = Map.Map String Type

  boolType = SType "Bool"
  integerType = SType "Integer"
  doubleType = SType "Double"

  typeOfLiteral :: String -> Type
  typeOfLiteral "True" = boolType
  typeOfLiteral "False" = boolType
  typeOfLiteral _ = doubleType

  typeOf :: TExpr -> Type
  typeOf (TLit t _) = t
  typeOf (TVar t _) = t
  typeOf (TBind t _ _) = t
  typeOf (TFun t _ _) = t

  --tradeType expected actual
  tradeType :: Type -> Type -> Maybe Type
  tradeType Any t = Just t
  tradeType t Any = Just t
  tradeType (FType f r) (FType ft rt) = do
    f' <- tradeType f ft
    r' <- tradeType r rt
    return (FType f' r')
  tradeType t1 t2
    | t1 == t2 = Just t1
    | otherwise = Nothing

  evaluateType :: SExpr -> Scope -> Type -> Maybe (TExpr, Scope)
  evaluateType (Lit l) scope expected = do
    let t = typeOfLiteral l
    guard (expected == Any || t == expected)
    return (TLit t l, scope)
  
  evaluateType (Var v) scope expected = do
    t <- Map.lookup v scope -- Check if v exists in the scope and get the type
    tr <- tradeType expected t
    let scope1 = Map.insert v tr scope
    return (TVar tr v, scope1)
  
  evaluateType (Bind (B v be) e) scope expected = do
    -- Evaluate binding type
    (bte, scope1) <- evaluateType be scope Any
    let scope2 = Map.insert v (typeOf bte) scope1

    -- Evaluate (check) expression type
    (te, scope3) <- evaluateType e scope2 expected

    -- Remove binding from scope
    let scope4 = Map.delete v scope3
    return (TBind (typeOf te) (TB v bte) te, scope4)
  
  evaluateType (Fun le re) scope expected = do
    -- We have something like `le re`. A left functional expression (le) applied
    -- to a right expression. We know that (assuming t = expected)
    -- le re :: t
    -- so,
    -- le :: t1 -> t, for any type t1
    -- so,
    -- re :: t1 = Any (let the recursion infer the type)
    -- le :: FType t1 t (usefull information for the recursion)
    
    (rte, scope1) <- evaluateType re scope Any
    
    (lte, scope2) <- evaluateType le scope1 (FType (typeOf rte) expected)

    let (FType _ t) = typeOf lte
    return (TFun t lte rte, scope2)

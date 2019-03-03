module Sharing.Semantic.Transform where

  import Control.Monad
  import qualified Data.Map as Map
  
  import Debug.Trace

  import Sharade.Semantic.Internal.Utils
  import Sharade.Parser.Syntax
  import Sharade.Semantic.Semantic
  
  type Scope = Map.Map String Type

  boolType = SType "Bool"
  numberType = SType "Number"

  typeOfLiteral :: String -> Type
  typeOfLiteral "True" = boolType
  typeOfLiteral "False" = boolType
  typeOfLiteral _ = numberType

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

    let (FType t1 t) = typeOf lte

    -- Maybe its neccesary to re-evaluate the right expression, if the types are
    -- not equal (Probably the case when rte is Any).
    (rte, scope2) <- if t1 /= (typeOf rte)
      then evaluateType re scope2 t1
      else return (lte, scope2)

    return (TFun t lte rte, scope2)
  
  translateDecl :: FDecl -> Scope -> Maybe TFDecl
  translateDecl (FDecl fname fargs e) globalScope = do
    -- Create the local scope, adding all the arguments
    let lscope = foldl (\scope y -> Map.insert y Any scope) globalScope fargs

    (te, lscope2) <- evaluateType e lscope Any

    -- Form the final function type
    let ftype = foldr (\x y -> FType (Map.findWithDefault Any x lscope2) y) (typeOf te) fargs
    
    return (TFDecl ftype fname fargs te)
  
  initGlobalScope :: Scope
  initGlobalScope = Map.fromList [
    ("(+)",  FType numberType (FType numberType numberType)) ]
    --("(-)",  FType numberType (FType numberType numberType))),
    --("(*)",  FType numberType (FType numberType numberType))),
    --("(/)",  FType numberType (FType numberType numberType))),
    --("(==)", FType numberType (FType numberType boolType))),
    --("(!=)", FType numberType (FType numberType boolType)))),
    --("(<)",  FType numberType (FType numberType boolType)))),
    --("(<=)", FType numberType (FType numberType boolType)))),
    --("(>)",  FType numberType (FType numberType boolType)))),
    --("(>=)", FType numberType (FType numberType boolType)))),
  --] 
  
  createBasicFType :: FDecl -> Maybe Type
  createBasicFType (FDecl fname fargs _) = do
    -- A function is well-defined if the name of the function does not appear on
    -- its arguments and if two arguments are not equal.
    guard (allUnique (fname:fargs))
    
    -- Form the function type full of any
    return $ foldr (\_ y -> FType Any y) Any fargs
  
  funNames [] = []
  funNames ((FDecl name _ _):fs) = name : funNames fs

  createGlobalScope :: [FDecl] -> Maybe Scope
  createGlobalScope fs = do
    guard (allUnique $ funNames fs)

    ts <- sequence $ map createBasicFType fs
    let scope = Map.fromList $ zip (funNames fs) ts

    return $ Map.union initGlobalScope scope

  translateProgram :: [FDecl] -> Maybe [TFDecl]
  translateProgram fs = do
    globalScope <- createGlobalScope fs

    sequence $ map (\x -> translateDecl x globalScope) fs

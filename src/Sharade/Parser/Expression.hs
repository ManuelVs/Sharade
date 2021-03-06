module Sharade.Parser.Expression where

  import Control.Monad (ap)

  import Text.Parsec hiding (State)
  import Text.Parsec.Indent
  import Text.Parsec.Number
  
  import Sharade.Parser.Syntax
  import Sharade.Parser.Lexer

  createExpr op lexp rexp = (App (App (Var prefixNot) lexp) rexp) where
    prefixNot = "(" ++ op ++ ")"
  
  expr :: IParser Expr
  expr = expr' 0
  
  expr' :: Int -> IParser Expr
  expr' 0 = do
    le <- expr' 1
    cont <- expr'' 0
    return (cont le)
  
  expr' 1 = do
    le <- expr' 2
    cont <- expr'' 1
    return (cont le)
  
  expr' 2 = do
    le <- expr' 3
    cont <- expr'' 2
    return (cont le)
  
  expr' 3 = do
    le <- expr' 4
    cont <- expr'' 3
    return (cont le)
  
  expr' 4 = do
    le <- expr' 5
    cont <- expr'' 4
    return (cont le)

  expr' 5 = bexpr
  
  expr'' :: Int -> IParser (Expr -> Expr)
  expr'' 0 =
    do
      reservedOp "?"
      rexp <- expr' 1
      cont <- expr'' 0
      return (\lexp -> createExpr "?" lexp (cont rexp))
    <%> return (\e -> e)
  
  expr'' 1 =
    do
      reservedOp "&&"
      rexp <- expr' 2
      cont <- expr'' 1
      return (\lexp -> createExpr "&&" lexp (cont rexp))
    <%>
    do
      reservedOp "||"
      rexp <- expr' 2
      cont <- expr'' 1
      return (\lexp -> createExpr "||" lexp (cont rexp))
      <%> return (\e -> e)

  expr'' 2 =
    do
      reservedOp "<"
      rexp <- expr' 3
      cont <- expr'' 2
      return (\lexp -> createExpr "<" lexp (cont rexp))
    <%>
    do
      reservedOp "<="
      rexp <- expr' 3
      cont <- expr'' 2
      return (\lexp -> createExpr "<=" lexp (cont rexp))
    <%>
    do
      reservedOp ">"
      rexp <- expr' 3
      cont <- expr'' 2
      return (\lexp -> createExpr ">" lexp (cont rexp))
    <%>
    do
      reservedOp ">="
      rexp <- expr' 3
      cont <- expr'' 2
      return (\lexp -> createExpr ">=" lexp (cont rexp))
    <%>
    do
      reservedOp "=="
      rexp <- expr' 3
      cont <- expr'' 2
      return (\lexp -> createExpr "==" lexp (cont rexp))
    <%>
    do
      reservedOp "!="
      rexp <- expr' 3
      cont <- expr'' 2
      return (\lexp -> createExpr "!=" lexp (cont rexp))
    <%> return (\e -> e)
  
  expr'' 3 =
    do
      reservedOp "+"
      rexp <- expr' 4
      cont <- expr'' 3
      return (\lexp -> createExpr "+" lexp (cont rexp))
    <%>
    do
      reservedOp "-"
      rexp <- expr' 4
      cont <- expr'' 3
      return (\lexp -> createExpr "-" lexp (cont rexp))
    <%> return (\e -> e)
  
  expr'' 4 =
    do
      reservedOp "*"
      rexp <- expr' 5
      cont <- expr'' 4
      return (\lexp -> createExpr "*" lexp (cont rexp))
    <%>
    do
      reservedOp "/"
      rexp <- expr' 5
      cont <- expr'' 4
      return (\lexp -> createExpr "/" lexp (cont rexp))
    <%> return (\e -> e)

  ------------------------------------------------------------------------------
  ------------------------------ BASIC EXPRESSIONS -----------------------------
  ------------------------------------------------------------------------------
  bexpr :: IParser Expr
  bexpr = lambda <%> choosein <%> letin <%> caseexpr <%> fexp <%> pair <%> parens expr

  aexp :: IParser Expr
  aexp = litExpr <%> variable <%> parens expr

  fexp :: IParser Expr
  fexp = do
    fs <- many1 aexp
    return (foldl1 App fs)

  litExpr :: IParser Expr
  litExpr = do
    l <- litValue
    return (Lit l)

  variable :: IParser Expr
  variable = do
    i <- identifier
    return (Var i)
  
  lambda :: IParser Expr
  lambda = do
    reservedOp "\\"
    args <- many identifier
    reservedOp "->"
    body <- expr
    return $ foldr Lam body args
  
  choosein :: IParser Expr
  choosein = do
    reserved "choose"
    i <- identifier
    reserved "="
    b <- expr
    reserved "in"
    e <- expr
    return (Ch i b e)
  
  letin :: IParser Expr
  letin = do
    reserved "let"
    x <- identifier
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return (Let x e1 e2)
  
  pair :: IParser Expr
  pair = parens $ do
    le <- expr
    reservedOp ","
    re <- expr
    return (App (App (Var "Pair") le) re)
  
  caseexpr :: IParser Expr
  caseexpr = do
    reserved "case"
    e <- expr
    reserved "of"
    ms <- block casematch
    return (Case e ms)
  
  casematch :: IParser Match
  casematch = do
    p <- pattern
    reservedOp "->"
    e <- expr
    semiColon
    return (Match p e)
  
  constructor :: IParser String
  constructor = do
    c <- upper
    cs <- many alphaNum
    spaces
    return (c:cs)

  pattern :: IParser Pattern
  pattern =
    do
      i <- constructor
      is <- many identifier
      return (PCon i (map PVar is))
    <%>
    do
      l <- litValue
      return (PLit l)
    <%>
    do
      i <- identifier
      return (PVar i)
    <%>
    parens (do
      le <- identifier
      reservedOp ","
      re <- identifier
      return (PCon "Pair" [PVar le, PVar re])
    )
  
  litValue :: IParser LitValue
  litValue =
    do
      eid <- decimalFloat
      spaces
      case eid of
        Left i -> return (IValue $ i)
        Right r -> return (DValue $ r)
    <%>
    do
      char '\''
      c <- anyChar
      char '\''
      spaces
      return (CValue c)
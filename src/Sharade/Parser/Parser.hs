module Sharade.Parser.Parser (
  parseDecl, parseExpr, parseModule
)where

  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Number

  import Sharade.Parser.Lexer
  import Sharade.Parser.Syntax
  
  createExpr op lexp rexp = (App (App (Var prefixNot) lexp) rexp) where
    prefixNot = "(" ++ op ++ ")"


  num :: Parser Expr
  num = do
    r <- floating <%> floating2 False
    spaces
    return (Lit $ show r)
  
  variable :: Parser Expr
  variable = do
    i <- identifier
    return (Var i)
  
  lambda :: Parser Expr
  lambda = do
    reservedOp "\\"
    args <- many identifier
    reservedOp "->"
    body <- expr
    return $ foldr Lam body args

  choosein :: Parser Expr
  choosein = do
    reserved "choose"
    i <- identifier
    reserved "="
    b <- expr
    reserved "in"
    e <- expr
    return (Ch i b e)

  letin :: Parser Expr
  letin = do
    reserved "let"
    x <- identifier
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return (Let x e1 e2)

  bexpr :: Parser Expr
  bexpr = lambda <%> choosein <%> letin <%> fexp <%> parens expr
  
  expr :: Parser Expr
  expr = expr' 0

  expr' :: Int -> Parser Expr
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

  expr' 3 = bexpr

  expr'' :: Int -> Parser (Expr -> Expr)
  expr'' 0 =
    do
      reservedOp "?"
      rexp <- expr' 1
      cont <- expr'' 0
      return (\lexp -> createExpr "?" lexp (cont rexp))
    <%>
    do
      return (\e -> e)
    
  expr'' 1 =
    do
      reservedOp "+"
      rexp <- expr' 2
      cont <- expr'' 1
      return (\lexp -> createExpr "+" lexp (cont rexp))
    <%>
    do
      reservedOp "-"
      rexp <- expr' 2
      cont <- expr'' 1
      return (\lexp -> createExpr "-" lexp (cont rexp))
    <%> return (\e -> e)

  expr'' 2 =
    do
      reservedOp "*"
      rexp <- expr' 3
      cont <- expr'' 2
      return (\lexp -> createExpr "*" lexp (cont rexp))
    <%>
    do
      reservedOp "/"
      rexp <- expr' 3
      cont <- expr'' 2
      return (\lexp -> createExpr "/" lexp (cont rexp))
    <%> return (\e -> e)
      
  fexp :: Parser Expr
  fexp = do
    fs <- many1 aexp
    return (foldl1 App fs)

  aexp :: Parser Expr
  aexp = num <%> variable

  funDecl :: Parser FDecl
  funDecl = do
    f <- identifier
    as <- many identifier
    reserved "="
    e <- expr
    return (f, foldr Lam e as)

  ------------------------------------------------------------------------------

  parseExpr :: String -> Either ParseError Expr
  parseExpr s = parse (contents expr) "" s

  parseDecl :: String -> Either ParseError FDecl
  parseDecl s = parse (contents $ funDecl) "" s

  parseModule :: String -> Either ParseError [FDecl]
  parseModule s = parse (contents $ many funDecl) "" s
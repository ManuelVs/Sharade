module Sharade.Parser.Parser (
  parseDecl, parseExpr
)where

  import Sharade.Parser.Syntax

  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Number
  import qualified Text.ParserCombinators.Parsec.Token as Tok

  langDef :: Tok.LanguageDef ()
  langDef = Tok.LanguageDef
    { Tok.commentStart    = "{-"
    , Tok.commentEnd      = "-}"
    , Tok.commentLine     = "#"
    , Tok.nestedComments  = True
    , Tok.identStart      = letter
    , Tok.identLetter     = alphaNum <|> oneOf "_'"
    , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedNames   = ["choose", "in"]
    , Tok.reservedOpNames = ["="]
    , Tok.caseSensitive   = True
    }

  lexer :: Tok.TokenParser ()
  lexer = Tok.makeTokenParser langDef

  parens :: Parser a -> Parser a
  parens = Tok.parens lexer

  reserved :: String -> Parser ()
  reserved = Tok.reserved lexer

  semiSep :: Parser a -> Parser [a]
  semiSep = Tok.semiSep lexer

  operator :: Parser String
  operator = Tok.operator lexer

  identifier :: Parser String
  identifier  = Tok.identifier lexer

  semiColon :: Parser String
  semiColon = Tok.semi lexer

  (<%>) :: Parser a -> Parser a -> Parser a
  infixr 1 <%>
  a <%> b = (try a) <|> b
  ------------------------------------------------------------------------------
  ------------------------------- END OF 'UTILS' -------------------------------
  ------------------------------------------------------------------------------

  createExpr op lexp rexp = (AFun (AFun (Var prefixNot) lexp) rexp) where
    prefixNot = "(" ++ op ++ ")"

  expr :: Parser SExpr
  expr =
    do
      reserved "choose"
      b <- bind
      reserved "in"
      e <- expr
      return (Bind b e)
    <%>
    do
      lexp <- fexp
      op <- operator
      rexp <- expr
      return $ createExpr op lexp rexp
    <%>
    do
      fexp

  fexp :: Parser SExpr
  fexp =
    do
      fs <- many1 aexp
      return (foldl1 AFun fs)

  aexp :: Parser SExpr
  aexp =
    do
      r <- num
      spaces
      return (Lit $ show r)
    <%>
    do
      i <- identifier
      return (Var i)

  num :: Parser Double
  num = floating <%> floating2 False
  
  bind :: Parser SBinding
  bind = do
    i <- identifier
    reserved "="
    e <- expr
    return (B i e)

  funDecl :: Parser FDecl
  funDecl = do
    f <- identifier
    as <- many identifier
    reserved "="
    e <- expr
    return (FDecl f as e)

  program :: Parser [FDecl]
  program =
    do
      d <- funDecl
      ds <- program

      return (d:ds)
    <%> return []
  ------------------------------------------------------------------------------

  contents :: Parser a -> Parser a
  contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

  parseExpr :: String -> Either ParseError SExpr
  parseExpr s = parse (contents expr) "" s

  parseDecl :: String -> Either ParseError FDecl
  parseDecl s = parse (contents funDecl) "" s
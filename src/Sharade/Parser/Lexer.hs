module Sharade.Parser.Lexer where

  import Text.ParserCombinators.Parsec
  import qualified Text.Parsec.Token as Tok
  
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
    , Tok.reservedOpNames = ["->", "\\", "="]
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

  reservedOp :: String -> Parser ()
  reservedOp = Tok.reservedOp lexer

  identifier :: Parser String
  identifier  = Tok.identifier lexer

  semiColon :: Parser String
  semiColon = Tok.semi lexer

  integer :: Parser Integer
  integer = Tok.integer lexer

  (<%>) :: Parser a -> Parser a -> Parser a
  infixr 1 <%>
  a <%> b = (try a) <|> b

  contents :: Parser a -> Parser a
  contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r
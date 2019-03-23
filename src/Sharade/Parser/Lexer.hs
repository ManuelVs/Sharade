module Sharade.Parser.Lexer where

  import Data.Functor.Identity
  import Control.Monad.State
  
  import Text.Parsec hiding (State)
  import Text.Parsec.Indent
  import qualified Text.Parsec.Token as Tok
  
  -- See <http://hackage.haskell.org/package/indents-0.5.0.0/docs/Text-Parsec-Indent.html>
  -- and <https://gist.github.com/sw17ch/2048516>
  type IParser a = IndentParser String () a
  
  langDef :: Tok.GenLanguageDef String () (IndentT Identity)
  langDef = Tok.LanguageDef
    { Tok.commentStart    = "{-"
    , Tok.commentEnd      = "-}"
    , Tok.commentLine     = "#"
    , Tok.nestedComments  = True
    , Tok.identStart      = letter
    , Tok.identLetter     = alphaNum <|> oneOf "_'"
    , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedNames   = ["choose", "let", "in", "case", "of"]
    , Tok.reservedOpNames = [",", "->", "\\", "=", "?", "&&", "||", "<", "<=", ">", ">=", "==", "!=", "+", "-", "*", "/"]
    , Tok.caseSensitive   = True
    }
  
  lexer :: Tok.GenTokenParser String () (IndentT Identity)
  lexer = Tok.makeTokenParser langDef
  
  parens :: IParser a -> IParser a
  parens = Tok.parens lexer
  
  reserved :: String -> IParser ()
  reserved = Tok.reserved lexer
  
  semiSep :: IParser a -> IParser [a]
  semiSep = Tok.semiSep lexer
  
  operator :: IParser String
  operator = Tok.operator lexer
  
  reservedOp :: String -> IParser ()
  reservedOp = Tok.reservedOp lexer
  
  identifier :: IParser String
  identifier  = Tok.identifier lexer
  
  semiColon :: IParser String
  semiColon = Tok.semi lexer
  
  integer :: IParser Integer
  integer = Tok.integer lexer
  
  (<%>) :: IParser a -> IParser a -> IParser a
  infixr 1 <%>
  a <%> b = (try a) <|> b
  
  contents :: IParser a -> IParser a
  contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r
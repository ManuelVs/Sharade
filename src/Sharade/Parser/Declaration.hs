module Sharade.Parser.Declaration where
  
  import Text.Parsec hiding (State)
  import Text.Parsec.Indent

  import Sharade.Parser.Syntax
  import Sharade.Parser.Lexer
  import Sharade.Parser.Expression

  funDecl :: IParser FDecl
  funDecl = do
    topLevel
    f <- identifier
    as <- many identifier
    reserved "="
    e <- expr
    semiColon
    return (f, Fix $ foldr Lam e (f:as))

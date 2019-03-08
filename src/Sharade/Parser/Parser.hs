module Sharade.Parser.Parser (
  parseDecl, parseExpr, parseModule
)where

  import Text.Parsec hiding (State)
  import Text.Parsec.Indent
  
  import Sharade.Parser.Syntax
  import Sharade.Parser.Lexer
  import Sharade.Parser.Expression
  import Sharade.Parser.Declaration

  iParse :: IParser a -> SourceName -> String -> Either ParseError a
  iParse aParser source_name input = runIndentParser aParser () source_name input
  
  parseExpr :: String -> Either ParseError Expr
  parseExpr s = iParse (contents expr) "" s
  
  parseDecl :: String -> Either ParseError FDecl
  parseDecl s = iParse (contents funDecl) "" s
  
  parseModule :: String -> Either ParseError [FDecl]
  parseModule s = iParse (contents $ block funDecl) "" s
  
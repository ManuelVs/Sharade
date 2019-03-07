import System.IO
import System.Environment

import Text.ParserCombinators.Parsec

import qualified Sharade.Parser.Parser as Parser
import qualified Sharade.Parser.Syntax as Syntax
import qualified Sharade.Translator.Translator as Translator

parseProgram :: String -> Either ParseError [Syntax.FDecl]
parseProgram pr = sequence $ map Parser.parseDecl (filter (not . null) $ lines pr)

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  let wpath = path ++ ".hs"

  fr <- openFile path ReadMode
  pr <- hGetContents fr
  
  case parseProgram pr of
    Left l -> print l
    Right r -> case Translator.translateModule r of
      Left l -> print l
      Right haskellProgram -> do
        fw <- openFile wpath WriteMode

        hPutStrLn fw "import Sharade.Prelude"
        hPutStr fw haskellProgram

        hClose fw

  hClose fr
  putStrLn "Done."

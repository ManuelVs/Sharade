import System.IO
import System.Environment

import qualified Sharade.Parser.Parser as Parser
import qualified Sharade.Semantic.Transform as Transform
import qualified Sharade.Translator.Eval as Eval

parseProgram pr = sequence $ map Parser.parseDecl (filter (not . null) $ lines pr)

main = do
  args <- getArgs
  let path = head args
  let wpath = path ++ ".hs"

  fr <- openFile path ReadMode
  pr <- hGetContents fr
  
  case parseProgram pr of
    Left l -> print l
    Right r -> do
      putStrLn "Transpiling..."
      let declarations = map Eval.translateDecl r
      
      fw <- openFile wpath WriteMode

      hPutStrLn fw "import Control.Monad"
      hPutStrLn fw "import Control.Monad.Sharing"
      mapM_ (hPutStrLn fw) declarations

      hClose fw
  hClose fr
  putStrLn "Done."

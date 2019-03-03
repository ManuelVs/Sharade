import System.IO
import System.Environment

import Sharade.Translator.Eval
import Sharade.Parser.Parser

parseProgram pr = sequence $ map parseDecl (filter (not . null) $ lines pr)

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
      let declarations = map translateDecl r
      
      fw <- openFile wpath WriteMode

      hPutStrLn fw "import Control.Monad"
      hPutStrLn fw "import Control.Monad.Sharing"
      mapM_ (hPutStrLn fw) declarations

      hClose fw
  hClose fr
  putStrLn "Done."

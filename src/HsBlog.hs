module HsBlog
  ( convertSingle,
    convertDirectory,
    process,
    buildIndex,
  )
where

import HsBlog.Convert (convert)
import HsBlog.Directory (buildIndex, convertDirectory)
import HsBlog.Env (defaultEnv)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import System.IO

convertSingle :: String -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: String -> String -> String
process title = Html.render . convert defaultEnv title . Markup.parse

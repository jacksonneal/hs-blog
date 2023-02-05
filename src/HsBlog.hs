module HsBlog where

import HsBlog.Convert (convert)
import qualified HsBlog.Html
import qualified HsBlog.Markup
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (getContents')

process :: HsBlog.Html.Title -> String -> String
process title = HsBlog.Html.render . convert title . HsBlog.Markup.parse

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      content <- getContents'
      putStrLn (process "Empty title" content)
    [input, output] ->
      do
        content <- readFile input
        exists <- doesFileExist output
        let writeResult = writeFile output (process input content)
        if exists
          then whenIO confirm writeResult
          else writeResult
    _ ->
      putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file]"

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. use y or n"
      confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()

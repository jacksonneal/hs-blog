{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import qualified HsBlog
import OptParse
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO

-- TODO: --replace flag

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      HsBlog.convertDirectory input output
    ConvertSingle input output ->
      let withInputHandle :: (String -> Handle -> IO a) -> IO a
          withInputHandle action =
            case input of
              Stdin ->
                action "" stdin
              InputFile file ->
                bracket
                  (openFile file ReadMode)
                  hClose
                  (action file)
          withOutputHandle :: (Handle -> IO a) -> IO a
          withOutputHandle action =
            case output of
              Stdout ->
                action stdout
              OutputFile file -> do
                exists <- doesFileExist file
                shouldOpenFile <-
                  if exists
                    then confirm
                    else pure True
                if shouldOpenFile
                  then
                    bracket
                      (openFile file WriteMode)
                      hClose
                      action
                  else exitFailure
       in withInputHandle (\title -> withOutputHandle . HsBlog.convertSingle title)

------------------------------------------------

-- * Utilities

-- | Confirm user action
confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)"
    *> getLine
    >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn "Invalid response. use y or n"
            *> confirm

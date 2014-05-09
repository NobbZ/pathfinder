module Main where

import           Control.Monad
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  boards <- getArgs
  boards <- filterM doesFileExist boards -- getArgs
  hPrint stderr boards
  mapM solve boards
  return ()

solve :: FilePath -> IO ()
solve fp = do
  putStrLn fp

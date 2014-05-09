module Main where

import           Control.Monad
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO

import           Data.Board
import           Data.WayTree

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  boardNames <- getArgs
  existingBoardNames <- filterM doesFileExist boardNames
  hPrint stderr existingBoardNames
  mapM solve existingBoardNames
  return ()

solve :: FilePath -> IO ()
solve fp = do
  putStrLn fp
  theBoard <- parseFile fp
  theTree <- liftM growTree $ return theBoard
  theWay <- liftM cut $ return theTree
  thePath <- liftM toList $ return theWay
  mapM print thePath
  return ()

parseFile :: FilePath -> IO Board
parseFile fp = do
  boardString <- readFile fp
  boardLines <- liftM lines $ return boardString
  hPrint stderr boardLines
  boardBoard <- liftM parseBoard $ return boardLines
  hPrint stderr boardBoard
  return boardBoard

parseBoard :: [String] -> Board
parseBoard [[]]   = []
parseBoard (l:[]) = [parseLine l]
parseBoard (l:ls) = [parseLine l] ++ parseBoard ls

parseLine l = map parseTile l
parseTile t = case t of
  'S' -> Start
  'E' -> Exit
  'X' -> Wall
  ' ' -> Way

growTree :: Board -> WayTree
growTree = const (Walkable (0,0) Start NotWalkable NotWalkable NotWalkable NotWalkable)

cut :: WayTree -> WayTree
cut = id

toList :: WayTree -> [Coords]
toList = const [(0,0)]


module Main where

import           Prelude            hiding (Left, Right)

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
growTree b = (Walkable (findStart b) Start (growTree' b ((findStart b) +: Up)) (growTree' b ((findStart b) +: Right)) (growTree' b ((findStart b) +: Down)) (growTree' b ((findStart b) +: Left)))
  -- const (Walkable (0,0) Start NotWalkable NotWalkable NotWalkable NotWalkable)
  where
    findStart b = head $ [(x,y) | x <- [0..length b], y <- [0..length (b!!x)], b!!x!!y == Start ]
    growTree' b (x, y) | x < 0 || y < 0 = NotWalkable
                       | b!!x!!y == Wall = NotWalkable
                       | otherwise    = Walkable (x,y) (b!!x!!y) (growTree' b ((x,y) +: Up)) (growTree' b ((x,y) +: Right)) (growTree' b ((x,y) +: Down)) (growTree' b ((x,y) +: Left))

cut :: WayTree -> WayTree
cut (Walkable c Start up right down left) = Walkable c Start (cut' [c] up) (cut' [c] right) (cut' [c] down) (cut' [c] left)
  where
    cut' _ (NotWalkable) = NotWalkable
    cut' _ (Walkable c Exit _ _ _ _) = Walkable c Exit NotWalkable NotWalkable NotWalkable NotWalkable
    cut' cs (Walkable (x,y) t up right down left) | x < 0 || y < 0  = NotWalkable
                                                  | (x,y) `elem` cs = NotWalkable
                                                  | otherwise       = Walkable (x,y) t (cut' ((x,y):cs) up) (cut' ((x,y):cs) right) (cut' ((x,y):cs) down) (cut' ((x,y):cs) left)

toList :: WayTree -> [Coords]
toList = const [(0,0)]


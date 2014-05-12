module Main where

import           Prelude            hiding (Left, Right)

import           Control.Monad
import           Data.List
import           Data.Tree
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
  thePath <- liftM (toList . cut . growTree) $ parseFile fp
  mapM (putStrLn . (\ (y,x) -> show x ++ "," ++ show y)) thePath
  return ()

parseFile :: FilePath -> IO Board
parseFile fp = liftM (parseBoard . lines) $ readFile fp

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
growTree b = Node (Walkable (findStart b) Start) (map (growTree' b (findStart b)) [Up, Right, Down, Left] )
  where
    findStart b = head $ [(x,y) | x <- [0..length b], y <- [0..length (b!!x)], b!!x!!y == Start]
    growTree' b (x,y) dir | x' < 0 || y' < 0 = Node (NotWalkable (x',y')) []
                          | b!!x'!!y' == Wall = Node (NotWalkable (x',y')) []
                          | otherwise = Node (Walkable (x',y') (b!!x'!!y')) (map (growTree' b (x',y')) [Up, Right, Down, Left] )
      where (x',y') = (x,y) +: dir

cut :: WayTree -> WayTree
cut (Node (Walkable c Start) dirs) = cut'' (Node (Walkable c Start) (map (cut' [c]) dirs))
  where
    cut' cs (Node (NotWalkable (x,y)) dirs) = Node (NotWalkable (x,y)) []
    cut' cs (Node (Walkable (x,y) Exit) _) = Node (Walkable (x,y) Exit) []
    cut' cs (Node (Walkable (x,y) t) dirs) | x < 0 || y < 0 = Node { rootLabel = NotWalkable (x,y)
                                                                   , subForest = []
                                                                   }
                                           | (x,y) `elem` cs = Node { rootLabel = NotWalkable (x,y)
                                                                    , subForest = []
                                                                    }
                                           | otherwise = Node { rootLabel = Walkable (x,y) t
                                                              , subForest = map (cut' ((x,y):cs)) dirs -- $ filter reachesExit dirs
                                                              }
    cut'' (Node (Walkable (x,y) t) dirs) = Node { rootLabel = Walkable (x,y) t
                                                , subForest = filter reachesExit dirs
                                                }
    cut'' t = t

reachesExit :: WayTree -> Bool
reachesExit (Node (Walkable _ Exit) _) = True
reachesExit (Node (NotWalkable _) _) = False
reachesExit (Node (Walkable _ _) dirs) = any reachesExit dirs

toList :: WayTree -> [Coords]
toList (Node (NotWalkable c) _) = []
toList (Node (Walkable c Exit) _) = [c]
toList (Node (Walkable c t) dirs) = c:toList ((maximumBy depthOrd) . filter reachesExit $ filter isWalkable dirs)

depthOrd :: Tree a -> Tree a -> Ordering
depthOrd l r | depth l < depth r = LT
             | depth l == depth r = EQ
             | otherwise         = GT

depth :: Tree a -> Int
depth (Node _ []) = 0
depth (Node _ ts) = 1 + maximum (map depth ts)

isWalkable :: WayTree -> Bool
isWalkable (Node (NotWalkable _) _) = False
isWalkable (Node (Walkable _ _) _)  = True

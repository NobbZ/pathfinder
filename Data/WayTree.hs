module Data.WayTree where

import           Data.Tree
import           Prelude    hiding (Left, Right)

import           Data.Board

data CellInfo = Walkable { getCoords :: Coords
                         , getTile   :: Tile
                         }
              | NotWalkable { getCoords :: Coords }
              deriving (Show)

type WayTree = Tree CellInfo

data Direction = Up
               | Right
               | Down
               | Left
               deriving (Show)

move :: Coords -> Direction -> Coords
move (x, y) Up    = (x  , y-1)
move (x, y) Right = (x+1, y  )
move (x, y) Down  = (x  , y+1)
move (x, y) Left  = (x-1, y  )

(+:) :: Coords -> Direction -> Coords
c +: d = move c d


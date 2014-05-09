module Data.Board where

data Tile = Start
          | Exit
          | Wall
          | Way
          deriving (Show, Eq)

type Board = [[Tile]]

type Coords = (Int, Int)

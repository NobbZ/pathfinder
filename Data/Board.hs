module Data.Board where

data Tile = Start
          | Exit
          | Wall
          | Way
          deriving (Show)

type Board = [[Tile]]


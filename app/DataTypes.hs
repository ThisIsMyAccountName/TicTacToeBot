module DataTypes where

import Data.Matrix (Matrix)

data Player = X | O deriving (Eq, Show)
data Result = Win | Draw | Loss | Ongoing deriving (Eq, Show, Ord)
data Cell = Empty | Taken Player deriving (Eq, Show)
type Board = Matrix Cell
type GameState = (Board, Player)
type Position = (Int, Int)
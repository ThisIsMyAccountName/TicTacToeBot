module DataTypes where

import Data.Matrix (Matrix)
import Discord.Types (UserId)
import Text.Megaparsec (Parsec)
import Data.Void (Void)

data Player = X | O deriving (Eq, Show)
data Result = Win | Draw | Loss | Ongoing deriving (Eq, Show, Ord)
data Cell = Empty | Taken Player deriving (Eq, Show)
type Board = Matrix Cell
type GameState = (Board, Player)
type Position = (Int, Int)

data Command
  = PlayBot
  | PlayUser UserId
  | Help
  deriving (Show, Eq)

type CommandParser = Parsec Void String
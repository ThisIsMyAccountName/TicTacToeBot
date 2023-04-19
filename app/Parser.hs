module Parser where
import Text.Megaparsec (Parsec, (<|>), try, parse, errorBundlePretty)
import Text.Megaparsec.Char (space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import DataTypes


playBotParser :: CommandParser Command
playBotParser = string "!play" >> pure PlayBot

playUserParser :: CommandParser Command
playUserParser = do
  _ <- string "!play"
  _ <- space1
  string "<@"
  playerId <- L.decimal
  return $ PlayUser playerId

helpParser :: CommandParser Command
helpParser = string "!help" >> pure Help

commandParser :: CommandParser Command
commandParser = try playUserParser <|> playBotParser <|> helpParser


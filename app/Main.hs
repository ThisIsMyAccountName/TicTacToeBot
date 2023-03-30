{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO)
import UnliftIO (liftIO)
import UnliftIO.Concurrent
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Matrix (Matrix, matrix, getElem, nrows, ncols, setElem, toList)
import System.Random (randomRIO)

import Discord
import Discord.Types
import Discord.Interactions
import qualified Discord.Requests as R

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, modifyTVar')

import DataTypes
import ExampleUtils (getToken)
import TicTacToe (boardToList, newGame, playMove, bestMove, isGameOver, availableMoves, isWinner, isDraw, playBotMove)

main :: IO ()
main = do
    tok <- getToken
    --connectFour newConnectFour
    gameStatesVar <- newTVarIO tictactoeState
    userFacingError <- runDiscord $ def
             { discordToken = tok
             , discordOnEvent = eventHandler gameStatesVar
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             }
    TIO.putStrLn userFacingError

eventHandler :: TVar (GameState) -> Event -> DiscordHandler ()
eventHandler gameStatesVar event = case event of
    Ready _ _ _ _ _ _ _ -> do
        echo "Bot ready"
        void $ restCall (R.CreateMessage 1084744206238621747 "<:pong:1084793665525919804>")
        pure () 
    InteractionCreate InteractionComponent {
      componentData = click@ButtonData {componentDataCustomId = (T.take 3 -> "ttt")},
      interactionUser = MemberOrUser user,
      ..
    } -> do
      let move = buttonClickToMove click
      currentGameStates <- liftIO $ readTVarIO gameStatesVar
      case move of 
        Nothing -> do
          case componentDataCustomId click of
            "ttt restart" -> do
              liftIO $ atomically $ modifyTVar' gameStatesVar  (\x-> newGame)
              editIntercation interactionId interactionToken ":x: to move" newGame True id 3 3
        Just move -> do
          let afterMove@(board,player) = playMove currentGameStates move
          if isWinner board X || isWinner board O
            then editIntercation interactionId interactionToken (if player == X then ":o: wins" else ":x: wins") afterMove True disableAllButtons 3 3
            else if isDraw board
              then editIntercation interactionId interactionToken "Draw" afterMove True disableAllButtons 3 3
              else do
                let afterBotMove@(botBoard, botPlayer) = playBotMove afterMove
                if isWinner botBoard X || isWinner botBoard O
                  then editIntercation interactionId interactionToken (if botPlayer == O then ":x: wins" else ":o: wins") afterBotMove True disableAllButtons 3 3
                  else if isDraw botBoard
                    then editIntercation interactionId interactionToken "Draw" afterBotMove True disableAllButtons 3 3
                    else do
                      liftIO $ atomically $ modifyTVar' gameStatesVar  (\x-> afterBotMove)
                      editIntercation interactionId interactionToken (if botPlayer == X then ":x: to move" else ":o: to move") afterBotMove True disableUserButtons 3 3
          pure ()
    MessageCreate m -> when (isCommand m && not (fromBot m)) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "<:pong:1084793665525919804>")
        currentGameStates <- liftIO $ readTVarIO gameStatesVar
        case messageContent m of
            "!play" -> do
                void $ restCall (R.CreateMessageDetailed (messageChannelId m) (
                  def { 
                        R.messageDetailedContent = ":x: to move",
                        R.messageDetailedComponents = Just $ boardToActionRows currentGameStates 3 3 ++ [restartButton]
                      }))
                pure ()
            "!help" -> sendMessage m "!play <move> - play a move"
            _ -> sendMessage m "invalid command\n!help to see all commands"

                      
    _ -> pure ()

-- GAME STATE STORAGE

tictactoeState :: GameState
tictactoeState = newGame

initialGameStates :: Map.Map UserId GameState
initialGameStates = Map.empty

updateGameState :: UserId -> GameState -> Map.Map UserId GameState -> Map.Map UserId GameState
updateGameState userID gameState gameStates = Map.insert userID gameState gameStates

getGameState :: UserId -> Map.Map UserId GameState -> Maybe GameState
getGameState userID gameStates = Map.lookup userID gameStates

updateMapState :: Message -> TVar (Map.Map UserId GameState) -> GameState -> DiscordHandler ()
updateMapState m gameStatesVar game = liftIO $ atomically $ modifyTVar' gameStatesVar (updateGameState (userId $ messageAuthor m) game)

-- UTILS

commands :: [Text]
commands = ["!play", "!help", "!"]

sendMessage :: Message -> Text -> DiscordHandler ()
sendMessage m msg = void $ restCall (R.CreateMessage (messageChannelId m) msg)

sendMessages :: Message -> [Text] -> DiscordHandler ()
sendMessages _ [] = pure ()
sendMessages m (msg:msgs) = do
  void $ restCall (R.CreateMessage (messageChannelId m) msg)
  sendMessages m msgs

packBoard :: GameState -> Text
packBoard state = T.concat $ (map T.pack $ boardToList $ fst state)

echo :: MonadIO m => Text -> m ()
echo = liftIO . TIO.putStrLn

showT :: Show a => a -> Text
showT = T.pack . show

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPrefix :: Message -> Bool
isPrefix = ("!" `isPrefixOf`) . toLower . messageContent

isCommand :: Message -> Bool
isCommand m = any (`isPrefixOf` toLower (messageContent m)) commands

pickRandomElem :: [a] -> IO a
pickRandomElem [] = error "Cannot pick an element from an empty list."
pickRandomElem xs = do
  index <- randomRIO (0, length xs - 1)
  return (xs !! index)

emptyBoard :: [ActionRow]
emptyBoard = (\y -> ActionRowButtons $ (\x -> Button (T.pack $ "ttt " <> show x <> show y) False ButtonStyleSecondary (Just "[ ]") Nothing) <$> [1 .. 3]) <$> [1 .. 3]

boardToActionRows :: GameState -> Int -> Int -> [ActionRow]
boardToActionRows (board, _) x y = 
  (\y -> ActionRowButtons $ 
  (\x -> Button 
    (T.pack $ "ttt " <> show x <> show y) 
    False 
    (colorForButton board x y) (Just $ buttonLogo board x y) 
    Nothing) 
    <$> [1 .. x]) <$> [1 .. y]

disableAllButtons :: [ActionRow] -> [ActionRow]
disableAllButtons = 
  map (\(ActionRowButtons buttons) -> 
    ActionRowButtons $ map 
    (\(Button cid _ style label _) -> 
      Button cid True style label Nothing) buttons)

disableUserButtons :: [ActionRow] -> [ActionRow]
disableUserButtons = 
  map (\(ActionRowButtons buttons) -> 
    ActionRowButtons $ map 
    (\(Button cid _ style label _) -> 
      if style /= ButtonStyleSecondary 
        then Button cid True style label Nothing 
        else Button cid False style label Nothing) 
        buttons)

restartButton :: ActionRow
restartButton = ActionRowButtons [Button "ttt restart" False ButtonStylePrimary (Just "Restart") Nothing]

colorForButton :: Board -> Int -> Int -> ButtonStyle
colorForButton board x y = 
  case getElem x y board of
    (Taken X) -> ButtonStyleSuccess
    (Taken O) -> ButtonStyleDanger
    Empty -> ButtonStyleSecondary

buttonLogo :: Board -> Int -> Int -> Text
buttonLogo board x y = 
  case getElem x y board of
    (Taken X) -> "❌"
    (Taken O) -> "⭕"
    Empty -> "-"

buttonClickToMove :: ComponentData -> Maybe (Int, Int)
buttonClickToMove (ButtonData cid) = 
  if T.length cid == 6 
    then Just (read ([T.index cid 4]) :: Int, read ([T.index cid 5]) :: Int)
    else Nothing


parseMove :: Text -> GameState -> Maybe (Int, Int)
parseMove input gameState = 
  case input of 
    "1" -> if (1,1) `elem` availableMoves gameState then Just (1,1) else Nothing
    "2" -> if (1,2) `elem` availableMoves gameState then Just (1,2) else Nothing
    "3" -> if (1,3) `elem` availableMoves gameState then Just (1,3) else Nothing
    "4" -> if (2,1) `elem` availableMoves gameState then Just (2,1) else Nothing
    "5" -> if (2,2) `elem` availableMoves gameState then Just (2,2) else Nothing
    "6" -> if (2,3) `elem` availableMoves gameState then Just (2,3) else Nothing
    "7" -> if (3,1) `elem` availableMoves gameState then Just (3,1) else Nothing
    "8" -> if (3,2) `elem` availableMoves gameState then Just (3,2) else Nothing
    "9" -> if (3,3) `elem` availableMoves gameState then Just (3,3) else Nothing
    _ -> Nothing

-- MAIN HELPERS

executeMove :: GameState -> (Int, Int) -> Message -> TVar (Map.Map UserId GameState) -> DiscordHandler ()
executeMove gameState@(board, player) move m gameStatesVar = do
  let afterMove@(board, _) = playMove gameState move
  case isGameOver board of
    (Just result) -> do
      case result of
        Win -> sendMessage m "you won!"
        Loss -> sendMessage m "you lost!"
        Draw -> sendMessage m "draw!"
      sendMessages m [packBoard afterMove, "starting new game...", packBoard newGame]
      updateMapState m gameStatesVar newGame
      pure ()
    (Nothing) -> do
      case player of
        X -> executeMove afterMove (bestMove afterMove) m gameStatesVar
        O -> do
          sendMessage m $ packBoard afterMove
          updateMapState m gameStatesVar afterMove
          pure ()

editIntercation :: InteractionId -> InteractionToken -> Text -> GameState -> Bool -> ([ActionRow] -> [ActionRow]) -> Int -> Int -> DiscordHandler ()
editIntercation interactionId interactionToken text game isDone func x y = 
  void $ restCall (R.CreateInteractionResponse interactionId interactionToken 
    (InteractionResponseUpdateMessage (interactionResponseMessageBasic text) {
      interactionResponseMessageComponents = 
        Just $ func (boardToActionRows game x y) ++ if isDone then [restartButton] else []}))

-- case getGameState (userId $ messageAuthor m) currentGameStates of 
--   Nothing -> do
--     updateMapState m gameStatesVar newGame
--     sendMessages m ["starting game... (you play as O)", packBoard newGame]
--   Just game -> do
--     let move = parseMove x game
--     case move of
--       Nothing -> sendMessage m "invalid move"
--       Just move -> do
--         executeMove game move m gameStatesVar

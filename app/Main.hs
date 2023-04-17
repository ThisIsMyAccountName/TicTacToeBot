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
import Text.Megaparsec (Parsec, (<|>), try, parse, errorBundlePretty)
import System.Random (randomRIO)

import Discord
import Discord.Types
import Discord.Interactions
import qualified Discord.Requests as R

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, modifyTVar')

import DataTypes
import ExampleUtils (getToken)
import TicTacToe (newGame, playMove, bestMove, isGameOver, availableMoves, isWinner, isDraw, playBotMove)
import Parser (commandParser)

main :: IO ()
main = do
    tok <- getToken
    gameStatesVar <- newTVarIO tictactoeState
    twoPlayerGameStatesVar <- newTVarIO twoPlayerGameStates
    userFacingError <- runDiscord $ def
             { discordToken = tok
             , discordOnEvent = eventHandler gameStatesVar twoPlayerGameStatesVar
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             }
    TIO.putStrLn userFacingError

eventHandler :: TVar (GameState) -> TVar (Map.Map UserId (UserId, GameState)) -> Event -> DiscordHandler ()
eventHandler gameStatesVar twoPlayerGameStatesVar event = case event of
    Ready _ _ _ _ _ _ _ -> do
        echo "Bot ready"
        void $ restCall (R.CreateMessage 1084744206238621747 "<:pong:1084793665525919804>")
        pure () 
    InteractionCreate InteractionComponent {
      componentData = click@ButtonData {componentDataCustomId = (T.take 3 -> "bot")},
      interactionUser = MemberOrUser user,
      ..
    } -> do
      let move = buttonClickToMove click
      let text = "bot "
      currentGameStates <- liftIO $ readTVarIO gameStatesVar
      echo $ componentDataCustomId click
      case move of 
        Nothing -> do
          case componentDataCustomId click of
            "bot restart" -> do
              liftIO $ atomically $ modifyTVar' gameStatesVar  (\x-> newGame)
              editIntercation interactionId interactionToken ":x: to move" newGame True id 3 3 text [restartButton]
        Just move -> do
          let afterMove@(board,player) = playMove currentGameStates move
          if isWinner board X || isWinner board O
            then editIntercation interactionId interactionToken (if player == X then ":o: wins" else ":x: wins") afterMove True disableAllButtons 3 3 text [restartButton]
            else if isDraw board
              then editIntercation interactionId interactionToken "Draw" afterMove True disableAllButtons 3 3 text [restartButton]
              else do
                let afterBotMove@(botBoard, botPlayer) = playBotMove afterMove
                if isWinner botBoard X || isWinner botBoard O
                  then editIntercation interactionId interactionToken (if botPlayer == O then ":x: wins" else ":o: wins") afterBotMove True disableAllButtons 3 3 text [restartButton]
                  else if isDraw botBoard
                    then editIntercation interactionId interactionToken "Draw" afterBotMove True disableAllButtons 3 3 text [restartButton]
                    else do
                      liftIO $ atomically $ modifyTVar' gameStatesVar  (\x-> afterBotMove)
                      editIntercation interactionId interactionToken (if botPlayer == X then ":x: to move" else ":o: to move") afterBotMove True disableUserButtons 3 3 text [restartButton]
          pure ()
    InteractionCreate InteractionComponent {
      componentData = click@ButtonData {componentDataCustomId = (T.take 3 -> "two")},
      interactionUser = MemberOrUser user,
      ..
    } -> do
      twoPlayerStates <- liftIO $ readTVarIO twoPlayerGameStatesVar
      case user of 
        (Right _) -> pure () 
        (Left guildMemb) -> do
          let interactUser = memberUser guildMemb
          case interactUser of 
            Nothing -> pure ()
            Just userID1 -> do
              case getTwoPlayerGameState (userId userID1) twoPlayerStates of
                Nothing -> pure ()
                Just (userID2, game@(board,_)) -> do
                  let move = buttonClickToMove click
                  let text = "two "
                  case move of 
                    Nothing -> do
                      case componentDataCustomId click of
                        "two restart" -> do
                          --updateTwoPlayerMapState (userId userID1) twoPlayerGameStatesVar (userID2, newGame)
                          --updateTwoPlayerMapState userID2 twoPlayerGameStatesVar ((userId userID1), newGame)
                          deleteTwoPlayerMapState (userId userID1) twoPlayerGameStatesVar
                          deleteTwoPlayerMapState userID2 twoPlayerGameStatesVar
                          editIntercation interactionId interactionToken ((showT $ userName userID1) <> " resigned the game!")  newGame True id 0 0 text []
                    Just move -> do
                      let afterMove@(board, player) = playMove game move
                      if isWinner board X || isWinner board O
                        then do
                          --updateTwoPlayerMapState (userId userID1) twoPlayerGameStatesVar (userID2, afterMove)
                          updateTwoPlayerMapState userID2 twoPlayerGameStatesVar ((userId userID1), afterMove)
                          editIntercation interactionId interactionToken (if player == X then (showT userID2 <> " Wins!") else (showT $ userName userID1 <> " Wins!")) afterMove True disableAllButtons 3 3 text [restartButtonTwoPlayer]
                        else if isDraw board
                          then do
                            --updateTwoPlayerMapState (userId userID1) twoPlayerGameStatesVar (userID2, afterMove)
                            updateTwoPlayerMapState userID2 twoPlayerGameStatesVar ((userId userID1), afterMove)
                            editIntercation interactionId interactionToken "Draw" afterMove True disableAllButtons 3 3 text [restartButtonTwoPlayer]
                          else do
                            --updateTwoPlayerMapState (userId userID1) twoPlayerGameStatesVar (userID2, afterMove)
                            updateTwoPlayerMapState userID2 twoPlayerGameStatesVar ((userId userID1), afterMove)
                            editIntercation interactionId interactionToken (if player == X then (showT $ userName userID1) else showT userID2) afterMove True disableUserButtons 3 3 text [restartButtonTwoPlayer]
                      pure ()
    MessageCreate m -> when (isCommand m && not (fromBot m)) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "<:pong:1084793665525919804>")
        currentGameStates <- liftIO $ readTVarIO gameStatesVar
        case parse commandParser "" (T.unpack $ messageContent m) of
          Left bundle -> do
            --echo $ errorBundlePretty bundle
            sendMessage m "Invalid command\n!help to see all commands"
          Right command ->
            case command of
              PlayBot -> do
                void $ restCall (R.CreateMessageDetailed (messageChannelId m) (
                  def { 
                        R.messageDetailedContent = ":x: to move against bot",
                        R.messageDetailedComponents = Just $ boardToActionRows currentGameStates 3 3 "bot " ++ [restartButton]
                      }))
                pure () 
              PlayUser playerID -> do
                updateTwoPlayerMapState (userId $ messageAuthor m) twoPlayerGameStatesVar (playerID, newGame)
                void $ restCall (R.CreateMessageDetailed (messageChannelId m) (
                  def { 
                        R.messageDetailedContent = userName (messageAuthor m) <> " to play as :x:",
                        R.messageDetailedComponents = Just $ boardToActionRows newGame 3 3 "two " ++ [restartButtonTwoPlayer]
                      }))
                
                pure()
              Help -> do
                sendMessage m "!play to play against bot\n!play @user to play against another user\n!help to see all commands"
                pure ()

                      
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

twoPlayerGameStates :: Map.Map UserId (UserId, GameState)
twoPlayerGameStates = Map.empty

updateTwoPlayerGameState :: UserId -> UserId -> GameState -> Map.Map UserId (UserId, GameState) -> Map.Map UserId (UserId, GameState)
updateTwoPlayerGameState userID1 userID2 gameState gameStates = Map.insert userID1 (userID2, gameState) gameStates

getTwoPlayerGameState :: UserId -> Map.Map UserId (UserId, GameState) -> Maybe (UserId, GameState)
getTwoPlayerGameState userID1 gameStates = Map.lookup userID1 gameStates

updateTwoPlayerMapState :: UserId -> TVar (Map.Map UserId (UserId, GameState)) -> (UserId, GameState) -> DiscordHandler ()
updateTwoPlayerMapState userID1 gameStatesVar (userID2, game) = liftIO $ atomically $ modifyTVar' gameStatesVar (updateTwoPlayerGameState userID1 userID2 game)

deleteTwoPlayerMapState :: UserId -> TVar (Map.Map UserId (UserId, GameState)) -> DiscordHandler ()
deleteTwoPlayerMapState userID1 gameStatesVar = liftIO $ atomically $ modifyTVar' gameStatesVar (Map.delete userID1)

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

emptyBoard :: String -> [ActionRow]
emptyBoard text = (\y -> ActionRowButtons $ (\x -> Button (T.pack $ text <> show x <> show y) False ButtonStyleSecondary (Just "[ ]") Nothing) <$> [1 .. 3]) <$> [1 .. 3]

boardToActionRows :: GameState -> Int -> Int -> String ->[ActionRow]
boardToActionRows _ 0 0 _ = []
boardToActionRows (board, _) x y text = 
  (\y -> ActionRowButtons $ 
  (\x -> Button 
    (T.pack $ text <> show x <> show y) 
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

restartButton ::  ActionRow
restartButton = ActionRowButtons [Button "bot restart" False ButtonStylePrimary (Just "Restart") Nothing]

restartButtonTwoPlayer ::  ActionRow
restartButtonTwoPlayer = ActionRowButtons [Button "two restart" False ButtonStylePrimary (Just "Resign") Nothing]

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


editIntercation :: InteractionId -> InteractionToken -> Text -> GameState -> Bool -> ([ActionRow] -> [ActionRow]) -> Int -> Int -> String -> [ActionRow] -> DiscordHandler ()
editIntercation interactionId interactionToken text game isDone func x y buttonMark restart = 
  void $ restCall (R.CreateInteractionResponse interactionId interactionToken 
    (InteractionResponseUpdateMessage (interactionResponseMessageBasic text) {
      interactionResponseMessageComponents = 
        Just $ func (boardToActionRows game x y buttonMark) ++ if isDone then restart else []}))

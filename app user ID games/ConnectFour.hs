module ConnectFour where

import Control.Monad.State
import Data.Matrix (matrix, getElem, nrows, ncols, setElem, toList, submatrix)
import Data.List (intersperse, maximumBy, minimumBy, findIndex, subsequences)
import Data.Ord (comparing)
import Text.Read (readMaybe)

import DataTypes

emptyConnectFour :: Board
emptyConnectFour = matrix 6 7 (const Empty)

newConnectFour :: GameState
newConnectFour = (emptyConnectFour, X)

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

makeConnectFourMove :: Position -> State GameState ()
makeConnectFourMove (r, c) = do
  (board, currentPlayer) <- get
  let column = submatrix 1 6 c c board
  let maybeRow = findIndex (== Empty) (reverse $ toList column)
  case maybeRow of
	Just row -> do
	  let newBoard = setElem (Taken currentPlayer) (6 - row, c) board
	  put (newBoard, nextPlayer currentPlayer)
	Nothing ->
	  pure ()

playConnectFourMove :: GameState -> Position -> GameState
playConnectFourMove gameState position = execState (makeConnectFourMove position) gameState

isWinningMove :: Board -> Player -> Position -> Bool
isWinningMove board player (r, c) = any (fourInARow player) directions
  where
	directions = [horizontal, vertical, diagonal1, diagonal2]
	horizontal = [(r, c') | c' <- [max 1 (c - 3) .. min 7 (c + 3)]]
	vertical = [(r', c) | r' <- [max 1 (r - 3) .. min 6 (r + 3)]]
	diagonal1 = [(r + dr, c + dr) | dr <- [-3 .. 3]]
	diagonal2 = [(r - dr, c + dr) | dr <- [-3 .. 3]]

	fourInARow :: Player -> [(Int, Int)] -> Bool
	fourInARow p cells = any (all (== Taken p)) (subsequencesOfSize 4 (map getCell cells))

	getCell :: Position -> Cell
	getCell (r', c') = if (r' >= 1 && r' <= 6) && (c' >= 1 && c' <= 7) then getElem r' c' board else Empty

checkResult :: GameState -> Result
checkResult (board, currentPlayer) =
  if any (isWinningMove board currentPlayer) allPositions
	then Win
	else if any (== Empty) (toList board)
	  then Ongoing
	  else Loss
  where
	allPositions = [(r, c) | r <- [1 .. 6], c <- [1 .. 7]]

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = [subseq | subseq <- subsequences xs, length subseq == n]

printBoard :: Board -> String
printBoard board = unlines [printRow r | r <- [1 .. 6]] ++ columnLabels
  where
	printRow r = concat [printCell (getElem r c board) | c <- [1 .. 7]]
	printCell Empty = ". "
	printCell (Taken X) = "X "
	printCell (Taken O) = "O "
	columnLabels = "1 2 3 4 5 6 7\n"

printGameState :: GameState -> IO ()
printGameState (board, player) = do
  putStrLn $ "Current player: " ++ show player
  putStrLn $ printBoard board

connectFour :: GameState -> IO ()
connectFour gameState@(board, player) = do
  printGameState gameState
  putStrLn "Enter the column number (1-7) to make a move:"
  input <- getLine
  case readMaybe input of
	Just column | column >= 1 && column <= 7 -> do
	  let gs@(newBoardState, player) = playConnectFourMove gameState (0, column) -- Row is not used in playMove
	  let result = checkResult gs
	  case result of
		Win -> do
		  putStrLn $ "Player " ++ show player ++ " wins!"
		  printGameState gs
		Draw -> do
		  putStrLn "It's a draw!"
		  printGameState gs
		Loss -> do
		  putStrLn "It's a loss!"
		  printGameState gs
		Ongoing -> connectFour gs
	_ -> do
	  putStrLn "Invalid input. Please enter a number between 1 and 7."
	  connectFour gameState


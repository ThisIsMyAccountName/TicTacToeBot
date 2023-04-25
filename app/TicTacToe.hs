module TicTacToe where

import Control.Monad.State
import Data.Matrix (matrix, getElem, nrows, ncols, setElem, toList)
import Data.List (intersperse, maximumBy, minimumBy)
import Data.Ord (comparing)
import System.Random

import DataTypes 

emptyBoard :: Board
emptyBoard = matrix 3 3 (const Empty)

newGame :: GameState
newGame = (emptyBoard, X)

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

makeMove :: Position -> State GameState ()
makeMove (r, c) = do
  (board, currentPlayer) <- get
  let cell = getElem r c board
  case cell of
    Empty -> do
      let newBoard = setElem (Taken currentPlayer) (r, c) board
      put (newBoard, nextPlayer currentPlayer)
    Taken _ ->
        pure ()

playMove :: GameState -> Position -> GameState
playMove gameState position = execState (makeMove position) gameState

isWinner :: Board -> Player -> Bool
isWinner board player =
  let 
      rows = [[(1,1),(1,2),(1,3)],[(2,1),(2,2),(2,3)],[(3,1),(3,2),(3,3)]]
      cols = [[(1,1),(2,1),(3,1)],[(1,2),(2,2),(3,2)],[(1,3),(2,3),(3,3)]]
      diags = [[(1,1),(2,2),(3,3)],[(1,3),(2,2),(3,1)]]
      lines = rows ++ cols ++ diags
  in any (allTaken player) lines
  where
    allTaken p line = all (\(r, c) -> getElem r c board == Taken p) line

isDraw :: Board -> Bool
isDraw board = not (any (== Empty) (toList board))

legalMove :: GameState -> Position -> Bool
legalMove (board, _) (r, c) = getElem r c board == Empty && isValidMove (r, c)

isValidMove :: Position -> Bool
isValidMove (row, col) =
  row >= 1 && row < 4 && col >= 1 && col < 4
   
minimax :: GameState -> (Int, Position)
minimax gameState@(board, player) =
  if isWinner board O
    then (1, (-1, -1))
  else if isWinner board X
    then (-1, (-1, -1))
  else if isDraw board
    then (0, (-1, -1))
  else do
    let moves = availableMoves gameState
        scores = map (\move -> (minimax (playMove gameState move), move)) moves
        bestMove = 
            if player == O
                then maximumBy (comparing fst) scores 
                else minimumBy (comparing fst) scores
        in (fst (fst bestMove), snd bestMove)

bestMove :: GameState -> Position
bestMove gameState = snd $ minimax gameState

playBotMove :: GameState -> GameState
playBotMove gameState = playMove gameState (bestMove gameState)

availableMoves :: GameState -> [Position]
availableMoves gameState@(board,_) = [ (r, c) | r <- [1 .. 3], c <- [1 .. 3], legalMove gameState (r, c)]

module Move ( Move (..), mkMove, makeMaybeMove) where

import Board
import qualified Data.Vector as V

data Move = ClearIt (Int, Int) | FlagIt (Int, Int)

-- Make a move based on what's passed
mkMove :: Move -> Board -> Board
mkMove (ClearIt (i, j)) board = clearSquare board (i,j)
mkMove (FlagIt (i, j)) board = flagSquare board (i,j)

-- Deal with Move wrapped in Maybe Monad (For Bot)
makeMaybeMove :: Maybe Move -> Board -> Board
makeMaybeMove (Just (ClearIt (i, j))) board = clearSquare board (i,j)
makeMaybeMove (Just (FlagIt (i, j))) board = flagSquare board (i,j)
makeMaybeMove Nothing board = board

-- Clears a square, changes board grid, state and mode
clearSquare :: Board -> (Int, Int) -> Board
clearSquare board@(Board grid INPLAY mode fcount) (i, j) = case getSquare grid (i, j) of
  (Square (i,j) CLEAR) -> case mines of
    0 -> foldl clearSquare (Board newGrid (setBoardState newGrid) mode fcount) (getSurroundingSquares (i, j))
    _ -> (Board newGrid2 (setBoardState newGrid) mode fcount)
   where
    newGrid = setSquare grid (i, j) (Square (i, j) (CLEARED mines))
    mines = numberOfMines newGrid (i, j)
    newGrid2 = setSquare grid (i, j) (Square (i, j) (CLEARED mines))
  (Square (i,j) MINE) -> (Board newGrid (setBoardState newGrid) mode fcount)
   where
      newGrid = setSquare grid (i, j) (Square (i, j) EXPLODE)
  (Square (i, j) (CLEARED x)) -> Board grid INPLAY mode fcount
clearSquare board _ = board

-- Flags a square, changes board grid, state, mode and flag count
flagSquare :: Board -> (Int, Int) -> Board
flagSquare board@(Board grid INPLAY mode fcount) (i, j) = case getSquare grid (i, j) of
  (Square (i, j) FLAGMINE) -> (Board newGrid (setBoardState newGrid) mode (fcount + 1))
      where newGrid = setSquare grid (i, j) (Square (i, j) MINE)
  (Square (i, j) FLAGCLEAR) -> (Board newGrid (setBoardState newGrid) mode (fcount + 1))
      where newGrid = setSquare grid (i, j) (Square (i, j) CLEAR)
  (Square (i, j) MINE) -> (Board newGrid (setBoardState newGrid) mode (fcount - 1))
      where newGrid = setSquare grid (i, j) (Square (i, j) FLAGMINE)
  (Square (i, j) CLEAR) -> (Board newGrid (setBoardState newGrid) mode (fcount - 1))
      where newGrid = setSquare grid (i, j) (Square (i, j) FLAGCLEAR)
  _ -> board
flagSquare board _ = board

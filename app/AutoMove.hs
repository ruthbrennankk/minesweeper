module AutoMove ( makeAutoMove ) where

import qualified Data.Vector as V
import Control.Applicative
import Board
import Move


makeAutoMove :: Board -> [Int] -> Board
makeAutoMove board@(Board grid INPLAY _ _) rs = makeMaybeMove (findMove board rs) board
makeAutoMove board _ = board

-- Looks for next move - It is either an obvious open, and obvious flag or a random move
findMove :: Board -> [Int] -> Maybe Move
findMove board@(Board grid _ _ _) rs = open <|> flag <|> rm
  where open = obviousOpen board
        flag = obviousFlag grid
        rm = randomMove rs grid

-- Uses random numbers as indices, if they square at the index is hidden (clear or a mine)
-- then clear that square (ignore clear or mine, autoplayer doesn't know that)
-- Otherwise if the square is flagged or already cleared try again for a random move
-- Finally if no move can be found return Nothing
randomMove :: [Int] -> Grid -> Maybe Move
randomMove (x : y : xs) grid = case getSquare grid (y,x) of
  (Square _ CLEAR) -> Just (ClearIt (y, x))
  (Square _ MINE) -> Just (ClearIt (y, x))
  (Square _ FLAGMINE) -> randomMove xs grid
  (Square _ FLAGCLEAR) -> randomMove xs grid
  (Square _ (CLEARED _)) -> randomMove xs grid
  _ -> Nothing
randomMove (x : xs) _ = Nothing

-- opens a square whose neighbours are already marked the correct number of times
obviousOpen :: Board -> Maybe Move
obviousOpen board@(Board grid _ _ _ ) = foldl fx Nothing grid
 where
  fx m r = m <|> foldl fy m r
  fy m (Square (i, j) _) = m <|> obviousOpen' (i, j) board

obviousOpen' :: (Int, Int) -> Board -> Maybe Move
obviousOpen' (i, j) board@(Board grid _ _ fcount) = case getSquare grid (i,j) of
  (Square _ (CLEARED neighbours)) -> if flagCount == neighbours then unknownSquare else Nothing
   where
      flagCount = numberOfFlags grid (i,j)
      unknownSquare = foldl (\m (x, y) -> m <|> (clearFirstUnknown grid (x,y))) Nothing (getSurroundingSquares (i, j))
  _ -> Nothing

clearFirstUnknown :: Grid -> (Int, Int) -> Maybe Move
clearFirstUnknown grid (i,j) = case getSquare grid (i,j) of
                             (Square _ MINE) -> Just (ClearIt (i,j))
                             (Square _ CLEAR) -> Just (ClearIt (i,j))
                             _                           -> Nothing

-- flags a square whose (clear x) number equals the number of unopened squares around it
obviousFlag :: Grid -> Maybe Move
obviousFlag grid = foldl fx Nothing grid
 where
  fx m r = foldl fy m r
  fy m (Square (i,j) _ ) = m <|> obviousFlag' (i, j) grid

obviousFlag' :: (Int, Int) -> Grid -> Maybe Move
obviousFlag' (i, j) grid = case getSquare grid (i,j) of
  (Square _ (CLEARED neighbours)) -> if unknownCount == neighbours then unknownSquare else Nothing
   where
    unknownCount = numberOfUnknown grid (i,j)
    unknownSquare = foldl (\m (x, y) -> m <|> (flagFirstUnknown grid (x,y))) Nothing (getSurroundingSquares (i, j))
  _ -> Nothing


flagFirstUnknown :: Grid -> (Int, Int) -> Maybe Move
flagFirstUnknown grid (i,j) = case getSquare grid (i,j) of
                             (Square _ MINE) -> Just (FlagIt (i,j))
                             (Square _ CLEAR) -> Just (FlagIt (i,j))
                             _                           -> Nothing


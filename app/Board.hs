module Board
  (
  BoardSquare(..), Board (..), Status (..), BoardMode (..), BoardState (..), Grid
  , width
  , getFCount, getSquare, getClearNumber, numberOfMines, numberOfFlags, numberOfUnknown, getSurroundingSquares
  , setSquare, setBoardState, setupBoard
  )
where

import qualified Data.Vector as V

data Board = Board {
    grid :: Grid,
    state :: BoardState,
    mode :: BoardMode,
    flagCount :: Int
}

type Grid = V.Vector (V.Vector BoardSquare)
data BoardState = LOST | INPLAY | WON
data BoardMode = CLEARING | FLAGGING

data BoardSquare = Square {
    position :: (Int, Int),
    squareState :: Status
}

data Status = MINE | CLEAR | FLAGMINE | FLAGCLEAR | EXPLODE | CLEARED Int

-- the grid is a square so it's dimenstions are width x width
width :: Int
width = 10

-- Getters
getFCount :: Board -> Int
getFCount (Board _ _ _ fcount) = fcount

getSquare :: Grid -> (Int, Int)-> BoardSquare
getSquare g (i,j) = (g V.! i) V.! j

getClearNumber :: Grid -> (Int, Int)-> Int
getClearNumber g (i,j) = case getSquare g (i,j) of
    (Square (i,j) (CLEARED x)) -> x
    _ -> 9

numberOfMines :: Grid -> (Int, Int) -> Int
numberOfMines g (i, j) = length $ filter fx (getSurroundingSquares (i, j))
 where
 fx (i, j) = case getSquare g (i,j) of
        (Square _ MINE) -> True
        (Square _ FLAGMINE) -> True
        _ -> False

numberOfFlags :: Grid -> (Int, Int) -> Int
numberOfFlags g (i, j) = length $ filter fx (getSurroundingSquares (i, j))
 where
 fx (i, j) = case getSquare g (i,j) of
        (Square _ FLAGCLEAR) -> True
        (Square _ FLAGMINE) -> True
        _ -> False

numberOfUnknown :: Grid -> (Int, Int) -> Int
numberOfUnknown g (i, j) = length $ filter fx (getSurroundingSquares (i, j))
 where
 fx (i, j) = case getSquare g (i,j) of
        (Square _ MINE) -> True
        (Square _ CLEAR) -> True
        _ -> False

getSurroundingSquares :: (Int, Int) -> [(Int, Int)]
getSurroundingSquares (i, j) = filter isValidIndex [(i-1,j-1),(i,j-1),(i+1,j-1),(i-1,j),(i+1,j),(i-1,j+1),(i,j+1),(i+1,j+1)]

-- Helpers
isValidIndex :: (Int, Int) -> Bool
isValidIndex (i, j) = i >= 0 && i < width && j >= 0 && j < width

stateOfSquare :: BoardState -> BoardSquare -> BoardState
stateOfSquare LOST _ = LOST
stateOfSquare _ (Square _ EXPLODE) = LOST
stateOfSquare _ (Square _ CLEAR) = INPLAY
stateOfSquare state _ = state

-- Setters
setSquare :: Grid -> (Int, Int) -> BoardSquare -> Grid
setSquare g (i, j) square = g V.// [(i, (g V.! i V.// [(j, square)]))]

setBoardState :: Grid -> BoardState
setBoardState grid = foldl fx WON grid
 where fx bstate r = foldl stateOfSquare bstate r

-- Setup
setupBoard :: [Int] -> Board
setupBoard rs = (Board finalg INPLAY CLEARING tmines)
 where
  finalg = generateGrid rs
  tmines = totalMines finalg

generateGrid :: [Int] -> Grid
generateGrid rs = V.generate width $ \i -> V.generate width $ \j -> let squareState = if (randomgrid V.! i) V.! j then MINE else CLEAR in  Square (i, j) squareState
 where
  randomgrid = V.generate width $ \i -> V.generate width $ \j -> (rs !! (width * i + j)) <= 1

totalMines :: Grid -> Int
totalMines g = V.sum(V.map fx g)
 where
 fx r = V.length $ V.filter fy r
 fy s = case s of
        (Square _ MINE) -> True
        (Square _ FLAGMINE) -> True
        _ -> False

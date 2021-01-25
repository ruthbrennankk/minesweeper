module Draw ( drawBoard, canvasSize ) where

import qualified Data.Vector as V
import           System.Random
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Data.IORef
import           Control.Monad.Trans ( liftIO )

import Board
import Move

canvasSize :: Int
canvasSize = 400


drawBoard :: Board -> Element -> UI ()
drawBoard (Board grid state _ _) c = do
  grid <- V.forM grid (\x -> V.forM x (\square -> drawGridSquare square c)) --always draw the grid
  case state of
    LOST -> do
      return c
          # set UI.textFont    "30px sans-serif"
          # set UI.strokeStyle "gray"
          # set UI.fillStyle   (UI.htmlColor "black")
      c # UI.strokeText "You Lose" (141,61)
      c # UI.fillText   "You Lose" (140,60)
      return ()
    WON -> do
      return c
          # set UI.textFont    "30px sans-serif"
          # set UI.strokeStyle "gray"
          # set UI.fillStyle   (UI.htmlColor "Green")
      c # UI.strokeText "You Win" (141,61)
      c # UI.fillText   "You Win" (140,60)
    _ -> return ()
  return ()

buffer :: Int
buffer = 1

drawGridSquare :: BoardSquare -> Element -> UI ()
drawGridSquare (Square (i, j) MINE) c = drawHiddenSquare (i,j) c
drawGridSquare (Square (i, j) CLEAR) c = drawHiddenSquare (i,j) c
drawGridSquare (Square (i, j) FLAGCLEAR) c = drawFlagSquare (i,j) c
drawGridSquare (Square (i, j) FLAGMINE) c = drawFlagSquare (i,j) c
drawGridSquare (Square (i, j) EXPLODE) c = do c # set' UI.fillStyle (UI.htmlColor "red")
                                              c # UI.fillRect (x,y) d d
                                               where
                                                 x = standardX (i,j)
                                                 y = standardY (i,j)
                                                 d = standardD (i,j)

drawGridSquare (Square (i, j) (CLEARED 0)) c = do c # set' UI.fillStyle (UI.htmlColor "white")
                                                  c # UI.fillRect (x,y) d d
                                                   where
                                                     x = standardX (i,j)
                                                     y = standardY (i,j)
                                                     d = standardD (i,j)

drawGridSquare (Square (i, j) (CLEARED neighbours)) c = do  c # set' UI.fillStyle (UI.htmlColor "white")
                                                            c # UI.fillRect (x,y) d d
                                                            c # set' UI.fillStyle (UI.htmlColor "black")
                                                            c # set' UI.textAlign (UI.Center)
                                                            c # set' UI.textFont "24px sans-serif"
                                                            c # UI.fillText (show neighbours) (x2, y2)
                                                          where
                                                            x = standardX (i,j)
                                                            y = standardY (i,j)
                                                            d = standardD (i,j)
                                                            x2 = fromIntegral ((j * (canvasSize `div` width)) + ((canvasSize `div` width) `div` 2))
                                                            y2 = fromIntegral ((i * (canvasSize `div` width)) + ((canvasSize `div` width) - 10))


drawHiddenSquare :: (Int, Int) -> Element -> UI ()
drawHiddenSquare (i,j) c = do c # set' UI.fillStyle (UI.htmlColor "gray")
                              c # UI.fillRect (x,y) d d
                            where
                               x = standardX (i,j)
                               y = standardY (i,j)
                               d = standardD (i,j)

drawFlagSquare :: (Int, Int) -> Element -> UI ()
drawFlagSquare (i,j) c = do c # set' UI.fillStyle (UI.htmlColor "yellow")
                            c # UI.fillRect (x,y) d d
                            c # set' UI.fillStyle (UI.htmlColor "white")
                            c # set' UI.textAlign (UI.Center)
                            c # set' UI.textFont "32px sans-serif"
                            c # UI.fillText "F" (x2, y2)
                             where
                               x = standardX (i,j)
                               y = standardY (i,j)
                               d = standardD (i,j)
                               x2 = fromIntegral ((j * (canvasSize `div` width)) + ((canvasSize `div` width) `div` 2))
                               y2 = fromIntegral ((i * (canvasSize `div` width)) + ((canvasSize `div` width) - 6))

standardX :: (Int, Int) -> Double
standardX (i, j) = fromIntegral (j * (canvasSize `div` width) + buffer)

standardY :: (Int, Int) -> Double
standardY (i, j) = fromIntegral (i * (canvasSize `div` width) + buffer)

standardD :: (Int, Int) -> Double
standardD (i, j) = fromIntegral (canvasSize `div` width - (buffer*2))
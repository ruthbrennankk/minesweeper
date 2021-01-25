module Main where

import qualified Data.Vector as V
import System.Random
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.IORef
import Control.Monad.Trans ( liftIO )

import AutoMove
import Board
import Draw
import Move

main :: IO ()
main = do
  g <- newStdGen
  let rs = randomRs (0, width-1) g :: [Int]
  startGUI defaultConfig (setup rs)


setup :: [Int] -> Window -> UI ()
setup rs window = do
  let board = setupBoard rs
  return window # set title "Minesweeper"

  -- active elements
  elBoard <- liftIO $ newIORef (board)
  elMode <- liftIO $ newIORef CLEARING
  elOffset <- liftIO $ newIORef 1
  elSetMode <- UI.button #. "button" #+ [string "Flag"]
  elAutoMove <- UI.button #. "button" #+ [string "Automove"]
  elNew <- UI.button #. "button" #+ [string "New Game"]
  elFlags <- UI.span # set UI.text (" Flags Left " ++ show (getFCount board))
  c <- UI.canvas
          # set UI.height canvasSize
          # set UI.width canvasSize
          # set style [("border", "solid black 1px"), ("background", "#000")]

  drawBoard board c

  getBody window
    #+ [ column [element c], element elSetMode, element elAutoMove, element elNew, element elFlags]

  -- UI functionality
  let
    handleCanvasClick CLEARING (i, j) = do
                daBoard <- liftIO $ readIORef elBoard
                let newBoard = mkMove (ClearIt ( round ( j / cdivs ) , round (i / cdivs))) daBoard
                liftIO $ writeIORef elBoard newBoard
                drawBoard newBoard c
              where cdivs = a / b
                    a = fromIntegral canvasSize :: Double
                    b = fromIntegral width :: Double
    handleCanvasClick FLAGGING (i, j) = do
                daBoard <- liftIO $ readIORef elBoard
                let newBoard = mkMove (FlagIt ( round ( j / cdivs ) , round (i / cdivs))) daBoard
                liftIO $ writeIORef elBoard newBoard
                element elFlags # set UI.text (" Flags Left " ++ show (getFCount newBoard))
                drawBoard newBoard c

    cdivs = a / b
    a = fromIntegral canvasSize :: Double
    b = fromIntegral width :: Double

    handleNewGame = do  offset <- liftIO $ readIORef elOffset
                        let newBoard = setupBoard (drop (offset * (width * width)) rs)
                        liftIO $ writeIORef elBoard newBoard
                        liftIO $ writeIORef elOffset (offset + 1)
                        element elFlags # set UI.text (" Flags Left " ++ show (getFCount newBoard))
                        c # UI.clearCanvas
                        drawBoard newBoard c

    handleAutoMove = do  daBoard <- liftIO $ readIORef elBoard
                         let newBoard = makeAutoMove daBoard rs
                         liftIO $ writeIORef elBoard newBoard
                         element elFlags # set UI.text (" Flags Left " ++ show (getFCount newBoard))
                         drawBoard newBoard c

    handleSetMode CLEARING = do element elSetMode # set text ("Open")
                                liftIO $ writeIORef elMode FLAGGING
    handleSetMode FLAGGING = do element elSetMode # set text ("Flag")
                                liftIO $ writeIORef elMode CLEARING

  on UI.click elSetMode $ \_ -> do
    mode <- liftIO $ readIORef elMode
    handleSetMode mode
  on UI.click elAutoMove $ \_ -> handleAutoMove
  on UI.click elNew $ \_ -> handleNewGame
  on UI.mousedown c $ \(i, j) -> do
    mode <- liftIO $ readIORef elMode
    handleCanvasClick mode (i,j)

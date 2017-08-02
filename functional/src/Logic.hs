module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game

isCoordCorrect = inRange ((0, 0), (n - 1, n - 1))

switchPlayer game =
    case gamePlayer game of
      PlayerX -> game { gamePlayer = PlayerO }
      PlayerO -> game { gamePlayer = PlayerX }

full :: [Cell] -> Maybe Player
full (x@(Just player):xs) | and $ map (== x) xs = Just player
full _                                          = Nothing

winner :: Board -> Maybe Player
winner board = asum $ map full cells
    where cells = map (map (board !)) coords :: [[Cell]]
          coords = rows ++ cols ++ diags

          rows  = [[(i,j) | i <- [0..n-1]] | j <- [0..n-1]]
          cols  = [[(j,i) | i <- [0..n-1]] | j <- [0..n-1]]
          diags = [[(i,i) | i <- [0..n-1]]
                  ,[(i,j) | i <- [0..n-1], let j = n-1-i ]]

countCells :: Cell -> Board -> Int
countCells cell = length . filter ((==) cell) . elems

checkGameOver game
    | Just p <- winner board =
        game { gameState = GameOver $ Just p }
    | countCells Nothing board == 0 =
        game { gameState = GameOver Nothing }
    | otherwise = game
    where board = gameBoard game

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isCoordCorrect cellCoord && board ! cellCoord == Nothing =
        checkGameOver
        $ switchPlayer
        $ game { gameBoard = board // [(cellCoord, Just player)] }
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame
transformGame _ game = game

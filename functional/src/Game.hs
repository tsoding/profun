module Game where

import Data.Array

data Player = PlayerX | PlayerO deriving (Eq, Show)
data Cell = Empty | Full Player deriving (Eq, Show)
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)

n :: Int
n = 3

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

initialGame = Game { gameBoard = (array indexRange $ zip (range indexRange) (cycle [Empty])) // [ ((0, 0), Full PlayerX)
                                                                                                , ((1, 1), Full PlayerX)
                                                                                                , ((2, 2), Full PlayerO)]
                   , gamePlayer = PlayerX
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))

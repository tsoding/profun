module Rendering where

import Data.Array

import Graphics.Gloss

import Game

boardGridColor = makeColorI 255 255 255 255
playerXColor = makeColorI 255 50 50 255
playerOColor = makeColorI 50 100 255 255
tieColor = greyN 0.5

boardAsRunningPicture board =
    pictures [ color playerXColor $ xCellsOfBoard board
             , color playerOColor $ oCellsOfBoard board
             , color boardGridColor $ boardGrid
             ]

outcomeColor (Just PlayerX) = playerXColor
outcomeColor (Just PlayerO) = playerOColor
outcomeColor Nothing = tieColor

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

xCell :: Picture
xCell = pictures [ rotate 45.0 $ rectangleSolid side 10.0
                 , rotate (-45.0) $ rectangleSolid side 10.0
                 ]
    where side = min cellWidth cellHeight * 0.75

oCell :: Picture
oCell = thickCircle radius 10.0
    where radius = min cellWidth cellHeight * 0.25

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

xCellsOfBoard :: Board -> Picture
xCellsOfBoard board = cellsOfBoard board (Just PlayerX) xCell

oCellsOfBoard :: Board -> Picture
oCellsOfBoard board = cellsOfBoard board (Just PlayerO) oCell

boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]

boardAsPicture board =
    pictures [ xCellsOfBoard board
             , oCellsOfBoard board
             , boardGrid
             ]

boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Running -> boardAsRunningPicture (gameBoard game)
                    GameOver winner -> boardAsGameOverPicture winner (gameBoard game)

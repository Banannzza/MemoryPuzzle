module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 1280
height = 960
offset = 100

startX, startY :: Float
startX = -350
startY = 250

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

genMatrixOfRect :: Float -> Float -> Float -> Color -> Float -> Float -> Float -> Float -> [Picture]
genMatrixOfRect stX stY count mColor szX szY intervalX intervalY = fst $ foldl(foldFunI) ([], (stX, stY)) [1..count]
    where
        genArrayOfRect stX stY = fst $ foldl(foldFunJ) ([], (stX, stY)) [1..count]
        getTranslate stX stY = translate stX stY $ color mColor $ rectangleSolid szX szY
        foldFunJ (list, (stX, stY)) x = (list ++ [getTranslate stX stY], (stX + intervalX, stY))
        foldFunI (list, (stX, stY)) x = (list ++ (genArrayOfRect stX stY)
            , (stX, stY - intervalY))
        
drawing :: Picture
drawing = pictures $ genMatrixOfRect startX startY 8 green 30 50 50 80
  where
    ballColor = red
    rectColor = green

main :: IO ()
main = display window background drawing
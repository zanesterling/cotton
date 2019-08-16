module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact

main :: IO ()
main = do
  screenSize <- getScreenSize
  play FullScreen
       black
       stepsPerSecond
       (World ["foo", "bar"])
       (drawWorld screenSize)
       handleEvent
       tickWorld
  where stepsPerSecond = 100

lineHeight = 100
linePadding = lineHeight * 0.3

drawWorld :: (Int, Int) -> World -> Picture
drawWorld (screenWidth, screenHeight) world =
  translate linePadding linePadding $ pictures $ map drawLine $ zip iota $ console world
  where
    drawLine (n, line) = newline n $ toBottomLeft $ color white $ text line
    toBottomLeft picture =
      translate (- (fromIntegral screenWidth) / 2)
                (- (fromIntegral screenHeight) / 2) $ picture
    newline n = translate 0 ((lineHeight + linePadding) * fromIntegral n)
    iota = let i n = n:i (n+1) in i 0



handleEvent :: Event -> World -> World
handleEvent (EventKey (Char c) Down _ _) world = typeChar c world
handleEvent (EventKey (SpecialKey key) Down _ _) world =
  if notSpecial key
  then typeChar (keyToChar key) world
  else case key of
    -- KeyEnter     -> newLine world
    -- KeyBackspace -> backspace world
    -- KeyDelete    -> backspace world
    otherwise    -> echoString (show key) world
  where
    notSpecial key = key == KeySpace
    keyToChar KeySpace = ' '
handleEvent _ world = world

tickWorld :: Float -> World -> World
tickWorld time world = world

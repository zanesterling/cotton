module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact

main :: IO ()
main = do
  (w, h) <- getScreenSize
  let screenSize = (w - 200, h - 200)
  play (InWindow "foo" screenSize (10, 10))
       black
       stepsPerSecond
       (World ["foo", "bar"])
       (drawWorld screenSize)
       handleEvent
       tickWorld
  where stepsPerSecond = 100

lineHeight = 10
linePadding = lineHeight * 0.3

drawWorld :: (Int, Int) -> World -> Picture
drawWorld (w, h) world =
  translate linePadding linePadding $
  translate (- fromIntegral w/2) (- fromIntegral h/2) $
  consolePicture
  where consolePicture = drawConsole (fromIntegral w, fromIntegral h) lineHeight $ console world

drawConsole :: (Float, Float) -> Float -> [String] -> Picture
drawConsole (w, h) fontSize console =
  pictures $ map drawLine $ zip iota $ console
  where
    drawLine (n, line) = newline n $ scale sc sc $ color white $ text line
    sc = fontSize * 0.01
    pad size = size * 1.3
    newline n = translate 0 (pad fontSize * fromIntegral n)
    iota = let i n = n:i (n+1) in i 0



handleEvent :: Event -> World -> World
handleEvent (EventKey (Char c) Down _ _) world =
  case c of
    '\b'      -> backspace world
    otherwise -> typeChar c world
handleEvent (EventKey (SpecialKey key) Down _ _) world =
  if notSpecial key
  then typeChar (keyToChar key) world
  else case key of
    KeyEnter     -> newLine world
    KeyBackspace -> backspace world
    KeyDelete    -> backspace world
    otherwise    -> echoString (show key) world
  where
    notSpecial key = key == KeySpace
    keyToChar KeySpace = ' '
handleEvent _ world = world

tickWorld :: Float -> World -> World
tickWorld time world = world

module Lib where

data World = World { console :: [String]
                   }

blankWorld :: World
blankWorld = World []

typeChar :: Char -> World -> World
typeChar c world = world { console = (firstLine ++ [c]):rest }
  where (firstLine:rest) = console world

echoString s world = world { console = firstLine:s:rest }
  where (firstLine:rest) = console world

backspace :: World -> World
backspace world = world { console = rest }
  where (firstLine:rest) = console world

newLine :: World -> World
newLine world = world { console = "" : console world}
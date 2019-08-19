module Lib where

data World = World { console :: [String]
                   , grid :: [[Bool]]
                   , gridSize :: (Int, Int)
                   }

blankWorld :: World
blankWorld = World []


-- Console manipulation

typeChar :: Char -> World -> World
typeChar c world = world { console = (firstLine ++ [c]):rest }
  where (firstLine:rest) = console world

echoString s world = world { console = firstLine:s:rest }
  where (firstLine:rest) = console world

backspace :: World -> World
backspace world = world { console = (dropLast firstLine):rest }
  where (firstLine:rest) = console world
        dropLast [] = []
        dropLast (_:[]) = []
        dropLast (x:xs) = x:dropLast xs

newLine :: World -> World
newLine world = world { console = "" : console world}


-- Graphics manipulation

setGrid :: Int -> Int -> a -> [[a]] -> [[a]]
setGrid x y a as = setAt y (setAt x a (as !! y)) as
  where setAt i x l = take i l ++ [x] ++ drop (i+1) l

paintPoint :: Int -> Int -> Bool -> World -> World
paintPoint x y on world =
  world { grid }

clearGrid world = world { grid = repeat y (repeat x false) }
  where (x, y) = gridSize world

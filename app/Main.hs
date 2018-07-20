module Main where

import Graphics.Gloss
--import Lib

main :: IO ()
main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)


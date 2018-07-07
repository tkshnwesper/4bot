module Main where

import Lib

main :: IO ()
main = print $ makeBoardGrid $ Board [0, 0, 1, 1, 2, 2, 3, 3, 4, 4]

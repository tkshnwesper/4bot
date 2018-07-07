module Main where

import Lib

main :: IO ()
main = print $ makeBoardGrid $ Board [1, 2, 1, 3, 1, 4, 1]

module Main where

import Lib

main :: IO ()
main = print $ makeBoardGrid $ Board [1, 2, 2, 3, 4, 3, 3, 4, 4, 5, 4]

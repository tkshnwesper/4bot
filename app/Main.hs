module Main where

import Data.List
import Lib

main :: IO ()
main =
    print (makeBoardGrid $ Board [1])
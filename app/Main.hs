module Main where

import Data.List
import Lib

main :: IO ()
main =
    let (BoardGrid array) = makeBoardGrid $ Board [4, 3, 3, 2, 1, 2, 2, 1, 1, 5, 1] in
    print $ BoardGrid (reverse . transpose $ array)

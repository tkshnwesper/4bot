module Lib (
    Board(..),
    hasWon
) where

import Data.Foldable

data BoardGridPlaceholder = Player1 | Player2 | Empty deriving (Eq)

instance Show BoardGridPlaceholder where
    show Player1 = "X"
    show Player2 = "O"
    show Empty = ""

newtype BoardGrid = BoardGrid [[BoardGridPlaceholder]] deriving (Eq, Show)
--
-- data BoardConfig = BoardConfig {
--     horizontalSlots :: Int,
--     verticalSlots :: Int
-- } deriving (Eq, Show)

newtype Board = Board {
--     config :: BoardConfig,
    moves :: [Int]
--     grid :: BoardGrid
} deriving (Eq, Show)

getPlayerBasedOnCount :: (Num a, Integral a) => a -> BoardGridPlaceholder
getPlayerBasedOnCount iterationCount
    | even iterationCount = Player1
    | otherwise = Player2

generateSliceMap :: (Num a, Eq a, Integral a) => [a] -> a -> [[BoardGridPlaceholder]]
generateSliceMap subArray count = let boardGrid = [] in case subArray of
    [] -> boardGrid
    (x:xs) -> [ if place == x then
                    getPlayerBasedOnCount count
                else
                    Empty
                | place <- [1..7]
              ] : generateSliceMap xs (count + 1)

squashSlicesInnerLoop :: Int -> [[BoardGridPlaceholder]] -> [BoardGridPlaceholder]
squashSlicesInnerLoop iteration arrayOfArrays =
    foldl (\placeHolders placeHolder ->
        if placeHolder == Empty then placeHolders else placeHolders ++ [placeHolder]
    ) [] $
        foldl (\captureArray sliceIndex ->
            ((arrayOfArrays !! sliceIndex) !! iteration) : captureArray) [] [0..(length arrayOfArrays - 1)]

squashSlices :: [[BoardGridPlaceholder]] -> [[BoardGridPlaceholder]]
squashSlices arrayOfArrays = foldl (\accumulator iteration ->
        squashSlicesInnerLoop iteration arrayOfArrays : accumulator
    ) [] [0..6]

makeBoardGrid :: Board -> BoardGrid
makeBoardGrid (Board array) = BoardGrid $ squashSlices $ generateSliceMap array 0

hasWon :: Board -> Bool
hasWon b = True

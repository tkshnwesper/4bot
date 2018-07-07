module Lib (
    Board(..),
    hasWon,
    makeBoardGrid,
    BoardGrid(..)
) where

import Data.Foldable
import Data.List

data BoardGridPlaceholder = Player1 | Player2 | Empty deriving (Eq)

instance Show BoardGridPlaceholder where
    show Player1 = "X"
    show Player2 = "O"
    show Empty = "_"

newtype BoardGrid = BoardGrid [[BoardGridPlaceholder]] deriving (Eq)

instance Show BoardGrid where
    show (BoardGrid arrayOfArrays) = foldl (\accumulator row ->
            let rowString = foldr ((++) . (++ " ") . show) "" row in
            if null accumulator then rowString else accumulator ++ "\n" ++ rowString
        ) "" arrayOfArrays
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

generateSlices :: (Num a, Eq a, Integral a) => [a] -> a -> [[BoardGridPlaceholder]]
generateSlices subArray count = let boardGrid = [] in case subArray of
    [] -> boardGrid
    (x:xs) -> [ if place == x then
                    getPlayerBasedOnCount count
                else
                    Empty
                | place <- [1..7]
              ] : generateSlices xs (count + 1)

squashSlicesInnerLoop :: Int -> [[BoardGridPlaceholder]] -> [BoardGridPlaceholder]
squashSlicesInnerLoop iteration arrayOfArrays =
    foldl (\placeHolders placeHolder ->
        if placeHolder == Empty then placeHolders else placeHolders ++ [placeHolder]
    ) [] $
        foldl (\captureArray sliceIndex ->
            captureArray ++ [(arrayOfArrays !! sliceIndex) !! iteration]) [] [0..(length arrayOfArrays - 1)]

transposeVerticalSlices :: [[BoardGridPlaceholder]] -> [[BoardGridPlaceholder]]
transposeVerticalSlices verticalSlices = foldl (\accumulator iterator ->
        foldl (\row sliceIndex ->
            let slice = (verticalSlices !! sliceIndex) in
            row ++ [if length slice - 1 >= iterator then slice !! iterator else Empty]
        ) [] [0..(length verticalSlices - 1)] : accumulator
    ) [] [0..foldr (max . length) 0 verticalSlices - 1]

squashSlices :: [[BoardGridPlaceholder]] -> [[BoardGridPlaceholder]]
squashSlices arrayOfArrays = transposeVerticalSlices $
    foldl (\accumulator iteration ->
        accumulator ++ [squashSlicesInnerLoop iteration arrayOfArrays]
    ) [] [0..6]

makeBoardGrid :: Board -> BoardGrid
makeBoardGrid (Board array) = BoardGrid $ squashSlices $ generateSlices array 0

checkHorizontalWinCondition :: BoardGrid -> BoardGridPlaceholder -> Bool
checkHorizontalWinCondition (BoardGrid arrayOfArrays) player =
    any (
        any (>= 4) . scanr (\currentElement count -> if currentElement == player then count + 1 else 0) 0
    ) arrayOfArrays

checkDiagonalWinCondition :: BoardGrid -> BoardGridPlaceholder -> Bool
checkDiagonalWinCondition (BoardGrid arrayOfArrays) player =
    let numberOfRows = length arrayOfArrays
        perimeter = if numberOfRows > 0 then numberOfRows + length (head arrayOfArrays) else 0 in
    any (\num ->
        any (>= 4)
        (scanr (\(x, y) accumulator ->
            let element = (arrayOfArrays !! y) !! x in
            if element == player then accumulator + 1 else 0
        ) 0
            (
                nub [(x, y) |
                x <- [0..num],
                y <- [0..num],
                x + y == num,
                x < length (head arrayOfArrays),
                y < length arrayOfArrays]
            )
        )
    ) [0..perimeter - 1]

hasWon :: Board -> Bool
hasWon board =
    let (BoardGrid arrayOfArrays) = makeBoardGrid board
        transposeArray = transpose arrayOfArrays in
    or $ [checkHorizontalWinCondition (BoardGrid arrayOfArrays),
    checkHorizontalWinCondition (BoardGrid transposeArray),
    checkDiagonalWinCondition (BoardGrid arrayOfArrays),
    checkDiagonalWinCondition (BoardGrid $ reverse transposeArray)] <*> [Player1, Player2]

import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
    describe "Connect4 Logic" $ do
        it "should win when identical colour is in 4 consecutive columns [Player 1]" $
            hasWon $ Board [0, 0, 1, 1, 2, 2, 3, 3, 4, 4]
        it "should win when identical colour is in 4 consecutive columns [Player 2]" $
            hasWon $ Board [5, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4]
        it "should win when identical colour is in 4 consecutive rows [Player 1]" $
            hasWon $ Board [1, 2, 1, 3, 1, 4, 1]
        it "should win when identical colour is in 4 consecutive rows [Player 2]" $
            hasWon $ Board [5, 1, 2, 1, 3, 1, 4, 1]
        it "should win when four identical colours are in diagonal consecutively [Player 1]" $
            hasWon $ Board [1, 2, 2, 3, 4, 3, 3, 4, 4, 5, 4]
        it "should win when four identical colours are in diagonal consecutively [Player 2]" $
            hasWon $ Board [6, 1, 2, 2, 3, 4, 3, 3, 4, 4, 5, 4]
        it "should win when four identical colours are in the opposite diagonal consecutively [Player 1]" $
            hasWon $ Board [4, 3, 3, 2, 1, 2, 2, 1, 1, 5, 1]
        it "should win when four identical colours are in the opposite diagonal consecutively [Player 2]" $
            hasWon $ Board [6, 4, 3, 3, 2, 1, 2, 2, 1, 1, 5, 1]

    describe "Displaying Board" $ do
        it "should print nothing when no moves have been made" $
            show (makeBoardGrid $ Board []) == ""
        it "should print X followed by _ when first move has been made in first column" $
            show (makeBoardGrid $ Board [1]) == "X _ _ _ _ _ _ "
        it "should print X O followed by _ when the second move has been made in second slot" $
            show (makeBoardGrid $ Board [1, 2]) == "X O _ _ _ _ _ "

import Test.Hspec

import Lib

main :: IO ()
main = hspec $
    describe "Connect4 Logic" $ do
        it "should win when identical colour is in 4 consecutive columns" $
            hasWon $ Board [0, 0, 1, 1, 2, 2, 3, 3, 4, 4]
        it "should win when identical colour is in 4 consecutive rows" $
            hasWon $ Board [1, 2, 1, 3, 1, 4, 1]
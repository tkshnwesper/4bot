import Test.Hspec

import Lib

main :: IO ()
main = hspec $
    describe "Connect4 Logic" $
        it "should win when identical colour is in 4 consecutive rows" $
            hasWon $ Board [0, 0, 1, 1, 2, 2, 3, 3, 4, 4]
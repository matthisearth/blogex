-- blogex
-- MainTest.hs

module Main (main) where

import OutsideInteract

main :: IO ()
main = withServer "processor/processing.js" $
    \m -> getResult m ["eqinline", "e^x"] >>= print

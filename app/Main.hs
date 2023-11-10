module Main (main) where

import qualified System.Environment as E
import qualified Data.List as L
import Runall

main :: IO ()
main = do
    as <- E.getArgs
    case as of
        [inDir, outDir] -> do
            let
                inDirStrip = (L.dropWhileEnd (== '/') inDir) ++ "/"
                outDirStrip = (L.dropWhileEnd (== '/') outDir) ++ "/"
            mainProcessing inDirStrip outDirStrip
        _ -> putStrLn "usage: blogex [indir] [outdir]"


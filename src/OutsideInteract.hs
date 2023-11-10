-- blogex
-- OutsideInteract.hs

module OutsideInteract (
    getResult ) where

import qualified System.Process as P

getResult :: [String] -> IO (Either String String)
getResult inputStrings = do
    stdout <- P.readProcess "blogexprocess" inputStrings ""
    case stdout of
        ('r':':':xs) -> return $ Right xs
        ('e':':':xs) -> return $ Left xs
        _ -> error "Error: Invalid server response."


-- blogex
-- FileHelpers.hs

module FileHelpers (
    withoutSuffix,
    isOneOfSuffix,
    texToHtmlPath,
    genRelPath ) where

import Data.Maybe (isJust)

-- All directories considered have a trailing slash and they are relative to
-- some base directory. Moreover, all paths must consist of ascii letters,
-- numbers, hyphens and dots.

-- The function withoutSuffix checks if the first argument is a suffix of the
-- second argument and if yes, it returns everything before the suffix

withoutSuffix :: String -> String -> Maybe String
withoutSuffix suffix str =
        if suffix == end
        then Just start
        else Nothing
    where
        n = length str - length suffix
        start = take n str
        end = drop n str

isOneOfSuffix :: [String] -> String -> Bool
isOneOfSuffix suffs str = or $ (\s -> isJust (withoutSuffix s str)) <$> suffs

texToHtmlPath :: String -> Int -> String
texToHtmlPath "index" 0 = ""
texToHtmlPath "index" n = (show n) ++ "/"
texToHtmlPath s 0 = case withoutSuffix "/index" s of
    Just t -> t ++ "/"
    Nothing -> s ++ "/"
texToHtmlPath s n = (texToHtmlPath s 0) ++ (show n) ++ "/"

-- Starts with a path to a directory and a path to a file or a directory and
-- computes relative path from the first to the second one.

genRelPath :: String -> String -> String
genRelPath start end =
    if dots ++ end == ""
        then "./"
        else dots ++ end
    where
        numDots = length $ filter (== '/') start
        dots = concat $ take numDots (repeat "../")


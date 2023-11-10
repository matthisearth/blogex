-- blogex
-- Runall.hs

module Runall (mainProcessing) where

import qualified System.Directory as D
import Control.Monad (filterM)
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Map as M
import Data.Map ((!))
import Parser
import Rendering
import OutsideInteract
import FileHelpers
import TemplateParser

-- Processing function

mainProcessing :: FilePath -> FilePath -> IO ()
mainProcessing inDir outDir = do
    (fs, ds) <- contentsRecursive inDir
    let
        outDs = (outDir ++) <$> ds
        texFsNoSuffix = mapMaybe (withoutSuffix ".tex") fs
        cssFsNoSuffix = mapMaybe (withoutSuffix ".css") fs
        otherFs = filter (not . (isOneOfSuffix [".tex", ".css", ".html"])) fs
        otherFsZip = zip ((inDir ++) <$> otherFs) ((outDir ++) <$> otherFs)
    -- Copy files
    mapM_ (D.createDirectoryIfMissing True) outDs
    mapM_ (\(a, b) -> D.copyFile a b) otherFsZip
    -- Filenames collect
    let
        cssFiles =  (++ ".css") <$> cssFsNoSuffix
        cssFilesAbsZip = (\s -> (inDir ++ s ++ ".css", outDir ++ s ++ ".css"))
            <$> cssFsNoSuffix
        texFiles = (\x -> (inDir, x)) <$> texFsNoSuffix
    -- Read template file
    template <- processTemplateFile inDir
    -- TeX parsing
    texDocs <- catMaybes <$> mapM parseTexFile texFiles
    -- Tex rendering
    let
        allDocs = filter (\(_, pd) -> hasDocClass (/= "config") pd) texDocs
        configTexDataList = filter (hasDocClass (== "config")) (snd <$> texDocs)
        cssContext = (\s -> [HereString s]) <$> cssFiles
        configTexDataNoCss = M.unions configTexDataList
        configTexData = M.insert "htmlcss" cssContext configTexDataNoCss
    let c = Context {
        thisPageData = M.empty,
        generalData = configTexData,
        allPageData = snd <$> allDocs }
    mapM_ (\(d, pd) -> renderTexFile outDir d (changeThisPage c pd) template) allDocs
    -- Generate css files after html files are written
    mapM_ (processCssFile c template outDir) cssFilesAbsZip

processTemplateFile :: FilePath -> IO (M.Map String [TemplateObject])
processTemplateFile inDir = do
    let templateFilename = inDir ++ "template.html"
    templateExists <- D.doesFileExist templateFilename
    templateString <- if templateExists
        then readFile templateFilename
        else return ""
    if templateString == ""
        then putStrLn $ "\"(templateparser) " ++ templateFilename ++ "\" Empty template."
        else return ()
    case buildTemplate templateFilename templateString of
        Right t -> return t
        Left e -> putStrLn (show e) >> return emptyTemplate

processCssFile :: Context -> Template -> FilePath -> (FilePath, FilePath) -> IO ()
processCssFile _ templ outDir (inCssPath, outCssPath) = do
    let postConfig = getTemplateValue "tailwindconfig" templ
    inCss <- readFile inCssPath
    outCss <- getResult ["postcss", outDir, inCss, postConfig]
    case outCss of
        Right s -> writeFile outCssPath s >> return ()
        -- Error printing consistent with parsec error printing
        Left e -> putStrLn $ "\"(tailwind) " ++ inCssPath ++ "\" " ++ e

parseTexFile :: (FilePath, FilePath) -> IO (Maybe (Doc, PageData))
parseTexFile (inDir, relInPath) = do
    let inPath = inDir ++ relInPath ++ ".tex"
    input <- readFile inPath
    let dataParsed = wholeParser inPath input
    case dataParsed of
        Right (d, pd) -> do
            let
                extras = M.fromList [
                    ("texpath", [[HereString relInPath]]),
                    ("htmlpath", [[HereString (texToHtmlPath relInPath 0)]]),
                    ("htmlcounter", [[HereString "0"]]) ]
            return $ Just (d, M.union pd extras)
        Left e -> putStrLn (show e) >> return Nothing

renderTexFile :: FilePath -> Doc -> Context -> Template -> IO ()
renderTexFile outDir d c template = do
    let
        texPath = case (thisPageData c) ! "texpath" of
            [[HereString s]] -> s
            _ -> ""
        n = case (thisPageData c) ! "htmlcounter" of
            [[HereString s]] -> read s :: Int
            _ -> 0
        htmlPath = texToHtmlPath texPath n
        newPd = M.insert "htmlpath" [[HereString htmlPath]] (thisPageData c)
    (html, another) <- renderFile d (changeThisPage c newPd) template
    D.createDirectoryIfMissing True (outDir ++ htmlPath)
    writeFile (outDir ++ htmlPath ++ "index.html") html
    if another
    then do
        let
            pd = thisPageData c
            countPd = M.insert "htmlcounter" [[HereString (show (n + 1))]] pd
        renderTexFile outDir d (changeThisPage c countPd) template
    else return ()

-- Perform a breadth first search in dirName and return all files in the first
-- and all directories in the second part of the tuple. The resulting paths do
-- not include the name of the directory. Directories (both in the input and the
-- output) have a trailing slash.

contentsRecursive :: FilePath -> IO ([FilePath], [FilePath])
contentsRecursive dirName =
    do
        allRel <- D.listDirectory dirName
        let allRelAbs = (\f -> (f, dirName ++ f)) <$> allRel
        dirsRelAbsNoSlash <- filterM (D.doesDirectoryExist . snd) allRelAbs
        let dirsRelAbs = (\(a, b) -> (a ++ "/", b ++ "/")) <$> dirsRelAbsNoSlash
        filesRelAbs <- filterM (D.doesFileExist . snd) allRelAbs
        allNextRelPre <- sequence $ contentsRecursivePrefix <$> dirsRelAbs
        let allNextRel = foldr addTuple ([], []) allNextRelPre
        let allNext = (fst allNextRel, snd allNextRel)
        return $ addTuple (fst <$> filesRelAbs, fst <$> dirsRelAbs) allNext
    where
        addTuple (a0, b0) (a1, b1) = (a0 ++ a1, b0 ++ b1)
        contentsRecursivePrefix (rel, absolute) = do
            (fs, ds) <- contentsRecursive absolute
            return ((rel ++) <$> fs, (rel ++) <$> ds)

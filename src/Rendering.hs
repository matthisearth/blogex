-- blogex
-- Rendering.hs

module Rendering (
    Doc(..),
    Block(..),
    Term(..),
    DocOrString(..),
    IsDocOrString(..),
    Context(..),
    PageData,
    hasDocClass,
    changeThisPage,
    specialSymbols,
    latexEnvironments,
    latexCommands,
    renderFile ) where

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.List as L
import qualified Data.Char as C
import OutsideInteract
import FileHelpers
import TemplateParser
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

-- Each field in a command or an environment can be either further parsed or
-- directly passed to the rendering function.
--
-- In an unprocessed environement, whenever encountering a backslash, the next
-- character is always parsed (which allows escaping of curly brackets,
-- percentage signs or backslashes). Removing the escape characters needs to be
-- done by the rendering functions.
--
-- Unescaped percentage signs % are comments and they together with the
-- remainder of the line (without the newline character) are always stripped
-- during the parsing process.
--
-- In constrast to standard LaTeX parsers, commands like item need to be
-- followed by a block in curly braces rather than just text.
--
-- Current documentclasses: page, post (appears in listings), config (not
-- rendered, only for global configuration).

-- Data types

newtype Doc = Doc [Block] deriving (Show, Eq)

newtype Block = Block [Term] deriving (Show, Eq)

data Term = SpecialSymbol String
    | OrdinaryChar Char
    | Command String [DocOrString]
    | Environment String [DocOrString]
    | BlockSeparator
    deriving (Show, Eq)

data DocOrString = HereDoc Doc
    | HereString String
    deriving (Show, Eq)

data IsDocOrString = IsDoc
    | IsString
    deriving (Show, Eq)

-- Context

data Context = Context {
    thisPageData :: PageData,
    generalData :: PageData,
    allPageData :: [PageData] }

type PageData = M.Map String [[DocOrString]]

hasDocClass :: (String -> Bool) -> PageData -> Bool
hasDocClass f pd = case M.lookup "documentclass" pd of
    Just [[HereString s]] -> if f s
        then True
        else False
    _ -> False

changeThisPage :: Context -> PageData -> Context
changeThisPage (Context _ g a) this = Context this g a

-- Processing things

-- Each command or enviornment is encoded as follows. In the example
-- \begin{code}{rust} ... \end{code} we have two string arguments. In this case,
-- we have codeArgs = [IsString, IsString] and the processing function 
-- codeProc :: [DocOrString] -> Context -> Template -> String only needs to be
-- defined on the relevant data types. Crucially, in the case of environements,
-- the first (not the last) argument corresponds to the contents of the
-- environment; this makes implementations simpler.

type TexArgs = [IsDocOrString]

type TexProcess = [DocOrString] -> Context -> Template -> IO String

justString :: String -> TexProcess
justString s _ _ _ = return s

wrapThings :: String -> String -> TexProcess
wrapThings start end (x:_) c templ = case x of
    (HereDoc d) -> do
        inner <- renderDoc d c templ
        return $ start ++ inner ++ end
    _ -> error "Rendering: Invalid call."
wrapThings _ _ [] _ _ = error "Rendering: Invalid call."

wrapThingsInline :: String -> String -> TexProcess
wrapThingsInline start end (x:_) c templ = case x of
    (HereDoc d) -> do
        inner <- renderDocInline d c templ
        return $ start ++ inner ++ end
    _ -> error "Rendering: Invalid call."
wrapThingsInline _ _ [] _ _ = error "Rendering: Invalid call."

concatIOString :: [IO String] -> IO String
concatIOString xs = concat <$> (sequence xs)

trimString :: String -> String
trimString = L.dropWhileEnd C.isSpace . L.dropWhile C.isSpace

renderWithTemplate :: Template -> String -> (M.Map String String) -> String
renderWithTemplate templ name m = case M.lookup name templ of
        Just xs -> concat (insertValue <$> xs)
        Nothing -> ""
    where
        insertValue (HtmlData s) = s
        insertValue (TexData s) = case M.lookup s m of
            Just v -> v
            Nothing -> ""

-- Rendering definitions; renderFile also returns a boolean value indicating
-- whether the page should be generated again with an increased counter (which
-- is needed for pages with multiple subpages).

renderFile :: Doc -> Context -> Template -> IO (String, Bool)
renderFile d c templ = do
    siteauthor <- renderGeneralContextInline c templ "siteauthor"
    headtitle <- renderPdInline c templ (thisPageData c) "headtitle"
    css <- printRelCss c
    sitetitle <- renderGeneralContextInline c templ "sitetitle"
    sitesubtitle <- renderGeneralContextInline c templ "sitesubtitle"
    menuitems <- renderMenuitems c templ
    pagetitle <- renderPdInline c templ (thisPageData c) "title"
    pagedate <- renderDate c templ
    pagecontent <- renderDoc d c templ
    sitefooter <- renderGeneralContextInline c templ "sitefooter"
    let 
        values = M.fromList [
            ("siteauthor", siteauthor),
            ("headtitle", headtitle),
            ("css", css),
            ("sitetitle", sitetitle),
            ("sitesubtitle", sitesubtitle),
            ("menuitems", menuitems),
            ("pagetitle", pagetitle),
            ("pagedate", pagedate),
            ("pagecontent", pagecontent),
            ("sitefooter", sitefooter) ]
        out = renderWithTemplate templ "doc" values
        sParsed :: Maybe Int
        sParsed = case M.lookup "manysplit" (thisPageData c) of
            Just [[HereString s]] -> readMaybe s
            _ -> Nothing
        htmlCounter :: Int
        htmlCounter = case M.lookup "htmlcounter" (thisPageData c) of
            Just [[HereString s]] -> read s
            _ -> error "Error: Html counter is always set."
        numPosts = length $ filter (hasDocClass (== "post")) (allPageData c)
    case sParsed of
        Just n -> return (out, n * htmlCounter < numPosts)
        Nothing -> return (out, False)

renderDoc :: Doc -> Context -> Template -> IO String
renderDoc (Doc bs) c templ = concatIOString $ (\b -> renderBlock b c templ) <$> bs

renderBlock :: Block -> Context -> Template -> IO String
renderBlock (Block [Environment s args]) c templ = renderTerm (Environment s args) c templ
renderBlock (Block ts) c templ = do
    content <- concatIOString $ (\t -> renderTerm t c templ) <$> ts
    return $ renderWithTemplate templ "block" $ M.fromList [("content", content)]

renderDocInline :: Doc -> Context -> Template -> IO String
renderDocInline (Doc [b]) c templ = renderBlockInline b c templ
renderDocInline _ _ _ = error "Rendering: Invalid call."

renderBlockInline :: Block -> Context -> Template -> IO String
renderBlockInline (Block bs) c templ = concatIOString $ (\t -> renderTerm t c templ) <$> bs

renderTerm :: Term -> Context -> Template -> IO String
renderTerm (SpecialSymbol s) _ _ = return $ specialSymbols ! s
renderTerm (OrdinaryChar c) _ _ = return [c]
renderTerm (Command n as) c templ = f as c templ
    where (_, f, _) = latexCommands ! n
renderTerm (Environment n as) c templ = f as c templ
    where (_, f, _) = latexEnvironments ! n
renderTerm BlockSeparator _ _ = return ""

specialSymbols :: M.Map String String
specialSymbols = M.fromList [
    ("--", "&ndash;"),
    ("`", "&apos;"),
    ("'", "&apos;") ]

-- The third part of the triple within latexCommands specifies True if the
-- command is a block level element. The same applies to latexEnvironments.
-- Importantly, if this argument is set to False, we need to call inline
-- rendering functions to make sure that there are no block level elements
-- within inline elements.

latexCommands :: M.Map String (TexArgs, TexProcess, Bool)
latexCommands = M.fromList [
    ("documentclass", ([IsString], justString "", True)), -- context
    ("headtitle", ([IsString], justString "", True)), -- context
    ("title", ([IsDoc], justString "", True)), -- context
    ("date", ([IsString], justString "", True)), -- context
    ("sitetitle", ([IsDoc], justString "", True)), -- context
    ("sitesubtitle", ([IsDoc], justString "", True)), -- context
    ("sitefooter", ([IsDoc], justString "", True)), -- context
    ("siteauthor", ([IsDoc], justString "", True)), -- context
    ("menuitem", ([IsString, IsDoc], justString "", True)), -- context
    ("manysplit", ([IsString], justString "", True)), -- context
    ("&", ([], justString "&amp;", False)),
    ("\"", ([], justString "&quot;", False)),
    ("\\", ([], justString "", False)),
    ("%", ([], justString "%", False)),
    ("$", ([], justString "$", False)),
    ("#", ([], justString "#", False)),
    ("postlist", ([], postlistProc, True)),
    ("postroll", ([], postrollProc, True)),
    ("section", ([IsDoc], wrapThingsInline "<h2>" "</h2>\n", True)),
    ("emph", ([IsDoc], wrapThingsInline "<em>" "</em>", False)),
    ("href", ([IsString, IsDoc], hrefProc, False)),
    ("link", ([IsString, IsDoc], linkProc, False)),
    ("caption", ([IsDoc], wrapThings "<div>" "</div>\n", True)),
    ("includegraphics", ([IsString], includegraphicProc, True)),
    ("icode", ([IsString, IsString], inlineCodeProc, False)),
    ("eqinline", ([IsString], eqinlineProc, False)),
    ("item", ([IsDoc], wrapThings "<li>" "</li>\n", True)) ]

latexEnvironments :: M.Map String (TexArgs, TexProcess, Bool)
latexEnvironments = M.fromList [
    ("abstract", ([IsDoc], justString "", True)), -- context
    ("document", ([IsDoc], wrapThings "" "", True)),
    ("center", ([IsDoc], wrapThings "<center>" "</center>\n", True)),
    ("figure", ([IsDoc], wrapThings "<figure>" "</figure>\n", True)),
    ("code", ([IsString, IsString], displayCodeProc, True)),
    ("eqdisplay", ([IsString], eqdisplayProc, True)),
    ("itemize",([IsDoc], wrapThings "<ul>" "</ul>\n", True)),
    ("enumerate", ([IsDoc], wrapThings "<ol>" "</ol>\n", True)) ]

postlistProc :: TexProcess
postlistProc _ c templ =
    do
        items <- concat <$> mapM postlistLink posts
        return $ renderWithTemplate templ "postlist" $ M.fromList [("items", items)]
    where
        posts = orderedPostList c
        postlistLink pd = do
            content <- renderPdInline c templ pd "title"
            date <- renderPdInline c templ pd "date"
            path <- renderPdInline c templ pd "htmlpath"
            let
                values = M.fromList [
                    ("relpath", createRelLink c path),
                    ("date", date),
                    ("content", content) ]
            return $ renderWithTemplate templ "postlistitem" values

postrollProc :: TexProcess
postrollProc _ c templ =
    case sParsed of
        Just m -> do
            let
                n = if htmlCounter == 0
                    then 0
                    else (htmlCounter - 1) * m
                fwdlink = if htmlCounter == 0
                    then "2/"
                    else "../" ++ (show (htmlCounter + 1)) ++ "/"
                forwardlink = if n + m < numPosts
                    then renderWithTemplate templ "postrollfwd" $ M.fromList [("fwdlink", fwdlink)]
                    else ""
                bwdlink = "../" ++ (show (htmlCounter - 1)) ++ "/"
                backwardlink = if htmlCounter > 1
                    then renderWithTemplate templ "postrollbwd" $ M.fromList [("bwdlink", bwdlink)]
                    else ""
            postrollitems <- concat <$> mapM postrollLink (take m (drop n posts))
            let
                values = M.fromList [
                    ("forwardlink", forwardlink),
                    ("backwardlink", backwardlink),
                    ("postrollitems", postrollitems) ]
            return $ renderWithTemplate templ "postroll" values
        Nothing -> return ""
    where
        sParsed :: Maybe Int
        sParsed = case M.lookup "manysplit" (thisPageData c) of
            Just [[HereString s]] -> readMaybe s
            _ -> Nothing
        htmlCounter :: Int
        htmlCounter = case M.lookup "htmlcounter" (thisPageData c) of
            Just [[HereString s]] -> read s
            _ -> error "Error: Html counter is always set."
        numPosts = length posts
        posts = orderedPostList c
        postrollLink pd = do
            content <- renderPdInline c templ pd "title"
            date <- renderPdInline c templ pd "date"
            path <- renderPdInline c templ pd "htmlpath"
            abstract <- renderPdInline c templ pd "abstract"
            let
                relpath = createRelLink c path
                values = M.fromList [
                    ("relpath", relpath),
                    ("content", content),
                    ("date", date),
                    ("abstract", abstract) ]
            return $ renderWithTemplate templ "postrollitem" values

hrefProc :: TexProcess
hrefProc [HereString s, HereDoc d] c templ = do
    content <- renderDocInline d c templ
    return $ "<a href=\"" ++ s ++ "\">" ++ content ++ "</a>"
hrefProc _ _ _ = error "Rendering: Invalid call."

linkProc :: TexProcess
linkProc [HereString s, HereDoc d] c templ = hrefProc [HereString link, HereDoc d] c templ
    where link = createRelLink c (texToHtmlPath s 0)
linkProc _ _ _ = error "Rendering: Invalid call."

includegraphicProc :: TexProcess
includegraphicProc [HereString s] _ _ = return $ "<img src=\"" ++ s ++ "\">\n"
includegraphicProc _ _ _ = error "Rendering: Invalid call."

removeBackEscape :: String -> String
removeBackEscape ('\\':x:xs) = x : removeBackEscape xs
removeBackEscape (x:xs) = x : removeBackEscape xs
removeBackEscape _ = ""

inlineCodeProc :: TexProcess
inlineCodeProc [HereString lang, HereString s] _ templ = do
    res <- getResult ["highlight", lang, trimString s]
    return $ case res of
        Right t -> renderWithTemplate templ "inlinecode" values
             where values = M.fromList [("lang", lang), ("code", removeBackEscape t)]
        Left _ -> errorString templ
inlineCodeProc _ _ _ = error "Rendering: Invalid call."

displayCodeProc :: TexProcess
displayCodeProc [HereString s, HereString lang] _ templ = do
    res <- getResult ["highlight", lang, trimString s]
    return $ case res of
        Right t -> renderWithTemplate templ "displaycode" values
             where values = M.fromList [("lang", lang), ("code", removeBackEscape t)]
        Left _ -> errorString templ
displayCodeProc _ _ _ = error "Rendering: Invalid call."

eqinlineProc :: TexProcess
eqinlineProc [HereString s] _ templ = do
    res <- getResult ["eqinline", trimString s]
    return $ case res of
        Right eq -> renderWithTemplate templ "eqinline" $ M.fromList [("eq", eq)]
        Left _ -> errorString templ
eqinlineProc _ _ _ = error "Rendering: Invalid call."

eqdisplayProc :: TexProcess
eqdisplayProc [HereString s] _ templ = do
    res <- getResult ["eqdisplay", trimString s]
    return $ case res of
        Right eq -> renderWithTemplate templ "eqdisplay" $ M.fromList [("eq", eq)]
        Left _ -> errorString templ
eqdisplayProc _ _ _ = error "Rendering: Invalid call."

errorString :: Template -> String
errorString templ = renderWithTemplate templ "errorstring" M.empty

-- Context processing

createRelLink :: Context -> String -> String
createRelLink c path = genRelPath htmlPath path
    where
        htmlPath = case (thisPageData c) ! "htmlpath" of
            [[HereString s]] -> s
            _ -> ""

printRelCss :: Context -> IO String
printRelCss c = return finalRender
    where
        cssNames = catMaybes $ extractCss <$> (generalData c) ! "htmlcss"
        extractCss [HereString s] = Just s
        extractCss _ = Nothing
        relCss = (createRelLink c) <$> cssNames
        renderStyle s = "<link rel=\"stylesheet\" href=\"" ++ s ++ "\">\n"
        finalRender = concat $ renderStyle <$> relCss

renderPdInline :: Context -> Template -> PageData -> String -> IO String
renderPdInline c templ pd key = case M.lookup key pd of
    Just docs -> case last docs of
        [HereDoc d] -> renderDocInline d c templ
        [HereString s] -> return s
        _ -> error "Rendering: Invalid call."
    Nothing -> return ""

renderGeneralContextInline :: Context -> Template -> String -> IO String
renderGeneralContextInline c templ key = case M.lookup key (generalData c) of
    Just docs -> case last docs of
        [HereDoc d] -> renderDocInline d c templ
        [HereString s] -> return s
        _ -> error "Rendering: Invalid call."
    Nothing -> return ""

renderMenuitems :: Context -> Template -> IO String
renderMenuitems c templ =
    case M.lookup "menuitem" (generalData c) of
        Just docs -> concat <$> (mapM extractPairs docs)
        Nothing -> return ""
    where
        extractPairs [HereString s, HereDoc d] = do
            content <- renderDocInline d c templ
            let
                values = M.fromList [
                    ("link", createRelLink c (texToHtmlPath s 0)),
                    ("content", content) ]
            return $ renderWithTemplate templ "menuitem" values
        extractPairs _ = error "Rendering: Invalid call."

renderDate :: Context -> Template -> IO String
renderDate c templ = do
    date <- renderPdInline c templ (thisPageData c) "date"
    if date == ""
    then return ""
    else return $ renderWithTemplate templ "date" $ M.fromList [ ("date", date) ]

reverseTimeOrder :: PageData -> PageData -> Ordering
reverseTimeOrder pd0 pd1 = compare (getPageDate pd1) (getPageDate pd0)
    where
        getPageDate pd = case getPageDateMaybe pd of
            Just d -> d
            Nothing -> (1970, 1, 1) -- unix epoch
        getPageDateMaybe pd = case M.lookup "date" pd of
            Just [[HereString (d0:d1:'/':m0:m1:'/':y0:y1:y2:y3:[])]] -> do
                day <- readMaybe [d0, d1] :: Maybe Int
                month <- readMaybe [m0, m1] :: Maybe Int
                year <- readMaybe [y0, y1, y2, y3] :: Maybe Int
                return (year, month, day)
            _ -> Nothing

orderedPostList :: Context -> [PageData]
orderedPostList c = L.sortBy reverseTimeOrder allPosts
    where allPosts = filter (hasDocClass (== "post")) (allPageData c)


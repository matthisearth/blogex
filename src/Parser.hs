-- blogex
-- Parser.hs

module Parser (wholeParser) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>), (<?>))
import qualified Data.Map as M
import Data.Map ((!))
import Rendering

-- Parsing helpers

lineRule :: P.Parsec String Int String
lineRule = P.manyTill P.anyChar $ P.try P.endOfLine
    <|> (P.try P.eof *> return '\n')

commentRule :: P.Parsec String Int String
commentRule = P.char '%' *> lineRule *> return "\n"

backslashRule :: P.Parsec String Int String
backslashRule = do
    back <- P.char '\\'
    c <- P.anyChar
    return $ [back, c]

untilUnescapedParser :: String -> P.Parsec String Int String
untilUnescapedParser t = P.try (P.string t *> return "")
    <|> P.try ((++) <$> backslashRule <*> untilUnescapedParser t)
    <|> ((:) <$> P.anyChar <*> untilUnescapedParser t)

collectArgs :: [IsDocOrString] -> P.Parsec String Int [DocOrString]
collectArgs (x:xs) = do
    _ <- P.char '{'
    arg <- collectArgUntil "}" x
    rest <- collectArgs xs
    return $ (arg:rest)
collectArgs _ = return []

collectArgUntil :: String -> IsDocOrString -> P.Parsec String Int DocOrString
collectArgUntil t IsString = HereString <$> untilUnescapedParser t
collectArgUntil t IsDoc = HereDoc <$> (docRule <* P.string t)

oneStringList :: [String] -> P.Parsec String Int String
oneStringList xs = P.choice $ (P.try . P.string) <$> xs

-- Consumes consecutive sequence of unicode whitespace characters, including
-- newlines and returns True if and only if two newlines appeared. This is the
-- only place where the internal Int state appears.

consumeSpaces :: P.Parsec String Int Term
consumeSpaces = do
    P.putState 0
    _ <- P.many1 (P.try eolMod <|> spaceMod)
    totNewline <- P.getState
    if totNewline > 1
    then return $ BlockSeparator
    else return $ OrdinaryChar ' '
    where
        eolMod = P.endOfLine *> P.modifyState (+1) *> return ()
        spaceMod = P.space *> return ()

-- Preparser rule

preparserRule :: P.Parsec String Int String
preparserRule = P.try ((++) <$> backslashRule <*> preparserRule)
    <|> P.try ((++) <$> commentRule <*> preparserRule)
    <|> (:) <$> P.anyChar <*> preparserRule
    <|> return ""

-- Parsing rules

-- The implementation below is of course incredibly crude but before
-- overoptimizing, let us see how well this rough method works in practice.
-- Indeed, the bottleneck is the execution of the nodejs things.

fileRule :: P.Parsec String Int Doc
fileRule = docRule <* P.eof

docRule :: P.Parsec String Int Doc
docRule = (Doc . (:[])) <$> blockRule -- mapping to make types work

blockRule :: P.Parsec String Int Block
blockRule = Block <$> (P.spaces *> P.many termRule)

termRule :: P.Parsec String Int Term
termRule = consumeSpaces
    <|> (specialSymbolRule <?> "special symbol")
    <|> (ordinaryCharRule  <?> "character")
    <|> (envRule <?> "environment")
    <|> (displayEqRule <?> "display equation")
    <|> (cmdRule <?> "command")
    <|> (inlineEqRule <?> "inline equation")

ordinaryCharRule :: P.Parsec String Int Term
ordinaryCharRule = do
    c <- P.noneOf "\\$%{}$&#<>~_&^" -- includes disallowed symbols
    return $ OrdinaryChar c

specialSymbolRule :: P.Parsec String Int Term
specialSymbolRule = do
    s <- oneStringList $ M.keys specialSymbols
    return $ SpecialSymbol s

envRule :: P.Parsec String Int Term
envRule = do
    name <- P.try $ P.string "\\begin{" *> nameParser <* P.string "}"
    let (envPar, _, _) = latexEnvironments ! name
    args <- collectArgs $ tail envPar
    content <- collectArgUntil ("\\end{" ++ name ++ "}") (head envPar)
    return $ Environment name (content:args)
    where nameParser = oneStringList $ M.keys latexEnvironments

cmdRule :: P.Parsec String Int Term
cmdRule = do
    name <- P.try $ P.string "\\" *> nameParser
    let (envPar, _, _) = latexCommands ! name
    args <- collectArgs envPar
    return $ Command name args
    where nameParser = oneStringList $ M.keys latexCommands

displayEqRule :: P.Parsec String Int Term
displayEqRule = do
    _ <- P.try $ P.string "$$"
    s <- untilUnescapedParser "$$"
    return $ Environment "eqdisplay" [HereString s]

inlineEqRule :: P.Parsec String Int Term
inlineEqRule = do
    _ <- P.try $ P.string "$" *> P.lookAhead (P.noneOf "$")
    s <- untilUnescapedParser "$"
    return $ Command "eqinline" [HereString s]

-- Parsing

mainParser :: String -> String -> Either P.ParseError Doc
mainParser name contents = do
    pre <- P.runParser preparserRule 0 ("(preparser) " ++ name) contents
    P.runParser fileRule 0 ("(parser) " ++ name) pre

-- Split into preamble attributes and document contents; furthermore, the
-- document contents are grouped into blocks.

wholeParser :: String -> String -> Either P.ParseError (Doc, PageData)
wholeParser name contents = splitFile <$> (mainParser name contents)

splitFile :: Doc -> (Doc, PageData)
splitFile (Doc [Block ts]) = (groupDoc mainDoc, snd collectDocPd)
    where
        mainDoc = case fst collectDocPd of
            Just d -> d
            Nothing -> Doc [Block []]
        collectDocPd = foldr addT (Nothing, M.fromList []) ts
        addT (Environment "document" [HereDoc d]) (Nothing, m) = (Just d, m)
        addT (Environment "document" [HereDoc _]) (Just x, m) = (Just x, m)
        addT (Environment "document" _) _ = error "Error: Impossible parsing."
        addT (Environment s d) (v, m) = (v, M.insertWith (++) s [d] m)
        addT (Command s d) (v, m) = (v, M.insertWith (++) s [d] m)
        addT _ (v, m) = (v, m)
splitFile _ = error "Error: Impossible parsing."

groupDoc :: Doc -> Doc
groupDoc (Doc [Block ts]) = Doc $ groupTerms ts
    where
        groupTerms [] = []
        groupTerms xs = if (next == []) || (next == [OrdinaryChar ' '])
                then restGrouped
                else (Block next):restGrouped
            where
                (next, rest) = collectBlock ([], xs)
                restGrouped = groupTerms rest
groupDoc _ = error "Error: Impossible parsing."

-- Moves terms from the left argument to the right argument until the term
-- BlockSeparator occurs.

collectBlock :: ([Term], [Term]) -> ([Term], [Term])
collectBlock (xs, []) = (xs, [])
collectBlock (xs, BlockSeparator:ys) = (xs, ys)
collectBlock (xs, (Environment s as):ys) =
    if isBlock
    then if xs == []
        then ([y], ys)
        else (xs, (Environment s as):ys)
    else collectBlock (xs ++ [y], ys)
    where
        (_, _, isBlock) = latexEnvironments ! s
        y = Environment s (groupDocString <$> as)
collectBlock (xs, (Command s as):ys) =
    if isBlock
    then if xs == []
        then ([y], ys)
        else (xs, (Command s as):ys)
    else collectBlock (xs ++ [y], ys)
    where
        (_, _, isBlock) = latexCommands ! s
        y = Command s (groupDocString <$> as)
collectBlock (xs, y:ys) = collectBlock (xs ++ [y], ys)

groupDocString :: DocOrString -> DocOrString
groupDocString (HereString s) = HereString s
groupDocString (HereDoc d) = HereDoc $ groupDoc d

-- blogex
-- TemplateParser.hs

module TemplateParser (
    TemplateObject(..),
    Template,
    buildTemplate,
    emptyTemplate,
    getTemplateValue ) where

import qualified Text.Parsec as P
import qualified Data.Map as M
import Text.Parsec ((<|>))

data TemplateObject = HtmlData String
    | TexData String
    deriving (Show, Eq)

type Template = M.Map String [TemplateObject]

-- Helper function

parseUntilDollar :: P.Parsec String () String
parseUntilDollar = P.manyTill P.anyChar (P.char '$')

getTemplateValue :: String -> Template -> String
getTemplateValue s templ = case M.lookup s templ of
    Just [HtmlData t] -> t
    _ -> ""

-- Main parsing

emptyTemplate :: Template
emptyTemplate = M.empty

buildTemplate :: String -> String -> Either P.ParseError Template
buildTemplate name contents = P.runParser templateParser ()
    ("(templateparser) " ++ name) contents

templateParser :: P.Parsec String () Template
templateParser = M.fromList <$> P.many environmentParser

environmentParser :: P.Parsec String () (String, [TemplateObject])
environmentParser = do
    name <- P.spaces *> P.string "$start$" *> parseUntilDollar
    xs <- P.many (P.try texDataParser <|> htmlDataParser)
    _ <- P.string ("$end$" ++ name ++ "$") <* P.spaces
    return (name, xs)

texDataParser :: P.Parsec String () TemplateObject
texDataParser = do
    s <- P.string "$here$" *> parseUntilDollar
    return $ TexData s

htmlDataParser :: P.Parsec String () TemplateObject
htmlDataParser = HtmlData <$> (P.many1 (escapedDollar <|> notDollar))
    where
        escapedDollar = P.try (P.string "\\$" *> return '$')
        notDollar = P.noneOf "$"

module Parser
    (block, document) where

import Control.Applicative
import Data.Char
import Data.Attoparsec.Text
import qualified Data.Text as T

import Subtext

------------------------------------------------------------
--                      Text Parsing                      --
------------------------------------------------------------

whitespace :: Parser T.Text
whitespace = takeWhile1 isSpace

word :: Parser T.Text
word = takeWhile1 $ not . isSpace

------------------------------------------------------------
--                     Inline Parsing                     --
------------------------------------------------------------

plainText :: Parser Inline
plainText = fmap PlainText $ word <|> whitespace

isUrlChar :: Char -> Bool
isUrlChar c = not $ c == '>' || isSpace c

string' :: String -> Parser T.Text
string' = string . T.pack

bareUrl :: Parser Inline
bareUrl = do
    schema <- string' "http" <|> string' "https"
    string' "://"
    body <- takeWhile1 isUrlChar
    let url = schema <> T.pack "://" <> body
    return $ BareUrl url

isAngledUrlChar :: Char -> Bool
isAngledUrlChar c = not $ c == '<' || c == '>' || isSpace c

angledUrl :: Parser Inline
angledUrl = do
    string' "<"
    url <- takeWhile1 isAngledUrlChar
    string' ">"
    return $ AngledUrl url

inline :: Parser Inline
inline = 
    bareUrl
    <|> angledUrl
    <|> plainText

inlines :: Parser [Inline]
inlines = many inline

------------------------------------------------------------
--                      Block Parsing                     --
------------------------------------------------------------

----------                 Helpers                ----------

prefixed :: Char -> Parser a -> Parser a
prefixed c parser = char c *> skipSpace *> parser

takeUntilEndOfLine :: Parser T.Text
takeUntilEndOfLine = do
    line <- takeWhile1 $ not . isEndOfLine
    endOfLine
    return line

----------              Block Parsing             ----------

paragraph :: Parser Block
paragraph = Paragraph <$> inlines

heading :: Parser Block
heading = Heading <$> prefixed '#' takeUntilEndOfLine

bullet :: Parser Block
bullet = Bullet <$> prefixed '-' inlines

quote :: Parser Block
quote = Quote <$> prefixed '>' inlines

blank :: Parser Block
blank = do
    endOfLine
    endOfLine
    return Blank

block :: Parser Block
block = 
    heading
    <|> bullet
    <|> quote
    <|> paragraph
    <|> blank

------------------------------------------------------------
--                    Document Parsing                    --
------------------------------------------------------------

document :: Parser Document
document = many block
module Subtextual.Parser
    (nonBlankBlock, document) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Text as T

import Subtextual.Core

------------------------------------------------------------
--                      Text Parsing                      --
------------------------------------------------------------

whitespace :: Parser T.Text
whitespace = takeWhile1 isHorizontalSpace <?> "whitespace"

word :: Parser T.Text
word = takeWhile1 (not . isSpace) <?> "word"

------------------------------------------------------------
--                     Inline Parsing                     --
------------------------------------------------------------

plainText :: Parser Inline
plainText = PlainText <$> (word <|> whitespace) <?> "plainText"

string' :: String -> Parser T.Text
string' = string . T.pack

bareUrl :: Parser Inline
bareUrl = do
    schema <- string' "https://" <|> string' "http://"
    body <- manyTill anyChar $ lookAhead endOfUrl
    let url = schema <> T.pack body
    return $ BareUrl url
    <?> "bareUrl"

    where
        endOfUrl :: Parser ()
        endOfUrl = 
            punctuationBoundary 
            <|> space $> () 
            <|> endOfInput

        punctuationBoundary :: Parser ()
        punctuationBoundary = do
            _ <- char '.' <|> char ';' <|> char ','
            _ <- skip isSpace <|> endOfLine
            return ()

isAngledUrlChar :: Char -> Bool
isAngledUrlChar c = not $ c == '<' || c == '>' || isSpace c

angledUrl :: Parser Inline
angledUrl = do
    _ <- string' "<"
    url <- takeWhile1 isAngledUrlChar
    _ <- string' ">"
    return $ AngledUrl url
    <?> "angledUrl"

isSlashLinkChar :: Char -> Bool
isSlashLinkChar c = 
    isAlpha c 
    || isDigit c 
    || c == '-' 
    || c == '_' 
    || c == '/' 

slashLink :: Parser Inline
slashLink = do
    _ <- char '/'
    link <- takeWhile1 isSlashLinkChar
    return $ SlashLink link
    <?> "slashLink"

inline :: Parser Inline
inline = 
    bareUrl
    <|> angledUrl
    <|> slashLink
    <|> plainText
    <?> "inline"

inlines :: Parser [Inline]
inlines = do
    parsed <- many1 inline
    let parsed' = smoosh parsed []
    return parsed'
    <?> "inlines"
    where
        smoosh :: [Inline] -> [Inline] -> [Inline]
        smoosh [] finished = reverse finished
        smoosh (PlainText p : todo) (PlainText p' : done) = 
            smoosh todo $ PlainText (p' <> p) : done
        smoosh (i : todo) done = smoosh todo (i : done)

------------------------------------------------------------
--                      Block Parsing                     --
------------------------------------------------------------

----------                 Helpers                ----------

prefixed :: Char -> Parser a -> Parser a
prefixed c parser = char c *> skipSpace *> parser

takeUntilEndOfLine :: Parser T.Text
takeUntilEndOfLine = takeWhile1 (not . isEndOfLine) <?> "takeUntilEndOfLine"

----------            Non-Blank Blocks            ----------

paragraph :: Parser Block
paragraph = Paragraph <$> inlines <?> "paragraph"

heading :: Parser Block
heading = Heading <$> prefixed '#' takeUntilEndOfLine <?> "heading"

bullet :: Parser Block
bullet = Bullet <$> prefixed '-' inlines <?> "bullet"

quote :: Parser Block
quote = Quote <$> prefixed '>' inlines <?> "quote"

tag :: Parser Block
tag = Tag <$> prefixed '!' word <?> "tag"

keyValue :: Parser Block
keyValue = prefixed '!' inner <?> "keyValue"
    where 
        inner :: Parser Block
        inner = do
            key <- word
            _ <- whitespace
            value <- takeUntilEndOfLine
            return $ KeyValue key value

triple :: Parser Block
triple = prefixed '&' inner <?> "triple"
    where
        inner :: Parser Block
        inner = do
            subject <- word
            _ <- whitespace
            predicate <- word
            _ <- whitespace
            object <- takeUntilEndOfLine
            return $ Triple subject predicate object

nonBlankBlock :: Parser Block
nonBlankBlock = 
    heading
    <|> bullet
    <|> quote
    <|> keyValue
    <|> tag
    <|> triple
    <|> paragraph
    <?> "nonBlankBlock"

nonBlankBlocks :: Parser Document
nonBlankBlocks = many1 nonBlankBlock <?> "nonBlankBlocks"

----------              Blank Blocks              ----------

newLines :: Parser Document
newLines = do
    eols <- many1 (Data.Attoparsec.Text.takeWhile isHorizontalSpace *> endOfLine)
    let len = length eols
    return $ replicate (len - 1) Blank
    <?> "newLines"

------------------------------------------------------------
--                    Document Parsing                    --
------------------------------------------------------------

document :: Parser Document
document = concat <$> many1 (nonBlankBlocks <|> newLines) <?> "document"
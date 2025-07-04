module Subtextual.Parser (parseNonBlankBlock, parseDocument) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Functor
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

parsePlainText :: Parser Inline
parsePlainText =
  PlainText
    <$> (word <|> whitespace)
    <?> "parsePlainText"

string' :: String -> Parser T.Text
string' = string . T.pack

parseBareUrl :: Parser Inline
parseBareUrl =
  do
    schema <- string' "https://" <|> string' "http://"
    body <- manyTill anyChar $ lookAhead endOfUrl
    let url = schema <> T.pack body
    return $ BareUrl url
    <?> "parseBareUrl"
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

parseAngledUrl :: Parser Inline
parseAngledUrl =
  do
    _ <- string' "<"
    url <- takeWhile1 isAngledUrlChar
    _ <- string' ">"
    return $ AngledUrl url
    <?> "parseAngledUrl"

parseSlashLink :: Parser Inline
parseSlashLink =
  do
    _ <- char '/'
    link <- takeWhile1 isSlashLinkChar
    return . SlashLink . DocumentName $ link
    <?> "parseSlashLink"

parseInline :: Parser Inline
parseInline =
  parseBareUrl
    <|> parseAngledUrl
    <|> parseSlashLink
    <|> parsePlainText
    <?> "parseInline"

parseInlines :: Parser [Inline]
parseInlines =
  do
    parsed <- many1 parseInline
    let parsed' = smoosh parsed []
    return parsed'
    <?> "parseInlines"
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
prefixed c parser =
  char c
    *> skipSpace
    *> parser
    <?> "prefixed"

takeUntilEndOfLine :: Parser T.Text
takeUntilEndOfLine =
  takeWhile1 (not . isEndOfLine)
    <?> "takeUntilEndOfLine"

----------            Non-Blank ABlocks            ----------

parseParagraph :: Parser BlockOrRef
parseParagraph =
  Left . Paragraph
    <$> parseInlines
    <?> "parseParagraph"

parseHeading :: Parser BlockOrRef
parseHeading =
  Left . Heading
    <$> prefixed '#' takeUntilEndOfLine
    <?> "parseHeading"

parseBullet :: Parser BlockOrRef
parseBullet =
  Left . Bullet
    <$> prefixed '-' parseInlines
    <?> "parseBullet"

parseQuote :: Parser BlockOrRef
parseQuote =
  Left . Quote
    <$> prefixed '>' parseInlines
    <?> "parseQuote"

parseTag :: Parser BlockOrRef
parseTag =
  Left . Tag
    <$> prefixed '!' word
    <?> "parseTag"

parseKeyValue :: Parser BlockOrRef
parseKeyValue = prefixed '!' inner <?> "parseKeyValue"
  where
    inner :: Parser BlockOrRef
    inner = do
      key <- word
      _ <- whitespace
      value <- takeUntilEndOfLine
      return . Left $ KeyValue key value

parseTriple :: Parser BlockOrRef
parseTriple = prefixed '&' inner <?> "triple"
  where
    inner :: Parser BlockOrRef
    inner = do
      subject <- word
      _ <- whitespace
      predicate <- word
      _ <- whitespace
      object <- takeUntilEndOfLine
      return . Left $ Triple subject predicate object

parseNonBlankBlock :: Parser BlockOrRef
parseNonBlankBlock =
  parseHeading
    <|> parseBullet
    <|> parseQuote
    <|> parseKeyValue
    <|> parseTag
    <|> parseTriple
    <|> parseParagraph
    <?> "parseNonBlankBlock"

parseNonBlankBlocks :: Parser [BlockOrRef]
parseNonBlankBlocks = many1 parseNonBlankBlock <?> "parseNonBlankABlocks"

----------              Blank ABlocks              ----------

parseNewLines :: Parser [BlockOrRef]
parseNewLines =
  do
    eols <- many1 (Data.Attoparsec.Text.takeWhile isHorizontalSpace *> endOfLine)
    let len = length eols
    return $ Left <$> replicate (len - 1) Blank
    <?> "parseNewLines"

------------------------------------------------------------
--                    Document Parsing                    --
------------------------------------------------------------

parseDocument :: Parser [BlockOrRef]
parseDocument =
  concat
    <$> many1 (parseNonBlankBlocks <|> parseNewLines)
    <?> "parseDocument"
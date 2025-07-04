module Subtextual.Parser (parseNonBlankBlock, parseBlockOrRefs) where

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

parseDocumentName :: Parser DocumentName
parseDocumentName = 
  DocumentName 
    <$> takeWhile1 isSlashLinkChar 
    <?> "parseDocumentName"

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
    docName <- parseDocumentName
    return . SlashLink $ docName
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

----------             Parsing Blocks             ----------

parseParagraph :: Parser Block
parseParagraph =
  Paragraph
    <$> parseInlines
    <?> "parseParagraph"

parseHeading :: Parser Block
parseHeading =
  Heading
    <$> prefixed '#' takeUntilEndOfLine
    <?> "parseHeading"

parseBullet :: Parser Block
parseBullet =
  Bullet
    <$> prefixed '-' parseInlines
    <?> "parseBullet"

parseQuote :: Parser Block
parseQuote =
  Quote
    <$> prefixed '>' parseInlines
    <?> "parseQuote"

parseTag :: Parser Block
parseTag =
  Tag
    <$> prefixed '!' word
    <?> "parseTag"

parseKeyValue :: Parser Block
parseKeyValue = prefixed '!' inner <?> "parseKeyValue"
  where
    inner :: Parser Block
    inner = do
      key <- word
      _ <- whitespace
      value <- takeUntilEndOfLine
      return $ KeyValue key value

parseTriple :: Parser Block
parseTriple = prefixed '&' inner <?> "triple"
  where
    inner :: Parser Block
    inner = do
      subject <- word
      _ <- whitespace
      predicate <- word
      _ <- whitespace
      object <- takeUntilEndOfLine
      return $ Triple subject predicate object

parseNonBlankBlock :: Parser Block
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
parseNonBlankBlocks = many1 (Left <$> parseNonBlankBlock) <?> "parseNonBlankBlocks"

----------          Parsing Blank Blocks          ----------

parseNewLines :: Parser [BlockOrRef]
parseNewLines =
  do
    eols <- many1 (Data.Attoparsec.Text.takeWhile isHorizontalSpace *> endOfLine)
    let len = length eols
    return $ Left <$> replicate (len - 1) Blank
    <?> "parseNewLines"

----------          Parsing Transclusions         ----------

parseWholeDocument :: Parser TransclusionOptions
parseWholeDocument = do
  _ <- skipSpace
  return WholeDocument
  <?> "parseWholeDocument"

parseFirstLines :: Parser TransclusionOptions
parseFirstLines = 
  FirstLines
  <$> prefixed '|' decimal
  <?> "parseFirstLines"

parseLines :: Parser TransclusionOptions
parseLines = prefixed '|' inner <?> "parseLines"
  where
    inner = do
      start <- decimal 
      _ <- skipSpace 
      length <- decimal
      return $ Lines start length

parseHeadingSection :: Parser TransclusionOptions
parseHeadingSection = 
  HeadingSection 
  <$> prefixed '#' takeUntilEndOfLine
  <?> "parseHeadingSection"

parseTransclusionOptions :: Parser TransclusionOptions
parseTransclusionOptions =
  parseLines
  <|> parseFirstLines
  <|> parseHeadingSection
  <|> parseWholeDocument
  <?> "parseTransclusionOptions"

parseTransclusion :: Parser Transclusion
parseTransclusion = prefixed '$' inner <?> "parseTransclusion"
  where
    inner = do
      docName <- parseDocumentName
      _ <- whitespace
      options <- parseTransclusionOptions
      _ <- takeUntilEndOfLine
      return $ Transclusion docName options

parseTransclusions :: Parser [BlockOrRef]
parseTransclusions = many1 (Right <$> parseTransclusion) <?> "parseTransclusions"

------------------------------------------------------------
--                    Document Parsing                    --
------------------------------------------------------------

parseBlockOrRefs :: Parser [BlockOrRef]
parseBlockOrRefs =
  concat
    <$> many1 (
        parseTransclusions
        <|> parseNonBlankBlocks
        <|> parseNewLines
      )
    <?> "parseBlockOrRefs"
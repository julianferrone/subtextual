module Subtextual.Parser (parseNonBlankBlock, parseTransclusion, parseAuthoreds) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Char as Char
import Data.Functor (($>))
import qualified Data.Text as Text
import qualified Subtextual.Core as Core

------------------------------------------------------------
--                      Text Parsing                      --
------------------------------------------------------------

whitespace :: Attoparsec.Parser Text.Text
whitespace = Attoparsec.takeWhile1 Attoparsec.isHorizontalSpace <?> "whitespace"

word :: Attoparsec.Parser Text.Text
word = Attoparsec.takeWhile1 (not . Char.isSpace) <?> "word"

------------------------------------------------------------
--                     Inline Parsing                     --
------------------------------------------------------------

parseDocumentName :: Attoparsec.Parser Core.DocumentName
parseDocumentName =
  Core.documentName
    <$> Attoparsec.takeWhile1 Core.isSlashLinkChar
    <?> "parseDocumentName"

parsePlainText :: Attoparsec.Parser Core.Inline
parsePlainText =
  Core.PlainText
    <$> (word <|> whitespace)
    <?> "parsePlainText"

string' :: String -> Attoparsec.Parser Text.Text
string' = Attoparsec.string . Text.pack

parseBareUrl :: Attoparsec.Parser Core.Inline
parseBareUrl =
  do
    schema <- string' "https://" <|> string' "http://"
    body <- manyTill Attoparsec.anyChar $ lookAhead endOfUrl
    let url = schema <> Text.pack body
    return $ Core.BareUrl url
    <?> "parseBareUrl"
  where
    endOfUrl :: Attoparsec.Parser ()
    endOfUrl =
      punctuationBoundary
        <|> Attoparsec.space $> ()
        <|> endOfInput

    punctuationBoundary :: Attoparsec.Parser ()
    punctuationBoundary = do
      _ <- Attoparsec.char '.' <|> Attoparsec.char ';' <|> Attoparsec.char ','
      _ <- Attoparsec.skip Char.isSpace <|> Attoparsec.endOfLine
      return ()

parseAngledUrl :: Attoparsec.Parser Core.Inline
parseAngledUrl =
  do
    _ <- string' "<"
    url <- Attoparsec.takeWhile1 Core.isAngledUrlChar
    _ <- string' ">"
    return $ Core.AngledUrl url
    <?> "parseAngledUrl"

parseSlashLink :: Attoparsec.Parser Core.Inline
parseSlashLink =
  do
    _ <- Attoparsec.char '/'
    docName <- parseDocumentName
    return . Core.SlashLink $ docName
    <?> "parseSlashLink"

parseInline :: Attoparsec.Parser Core.Inline
parseInline =
  parseBareUrl
    <|> parseAngledUrl
    <|> parseSlashLink
    <|> parsePlainText
    <?> "parseInline"

parseInlines :: Attoparsec.Parser [Core.Inline]
parseInlines =
  do
    parsed <- many1 parseInline
    let parsed' = smoosh parsed []
    return parsed'
    <?> "parseInlines"
  where
    smoosh :: [Core.Inline] -> [Core.Inline] -> [Core.Inline]
    smoosh [] finished = reverse finished
    smoosh (Core.PlainText p : todo) (Core.PlainText p' : done) =
      smoosh todo $ Core.PlainText (p' <> p) : done
    smoosh (i : todo) done = smoosh todo (i : done)

------------------------------------------------------------
--                      Block Parsing                     --
------------------------------------------------------------

----------                 Helpers                ----------

prefixed :: Char -> Attoparsec.Parser a -> Attoparsec.Parser a
prefixed c parser =
  Attoparsec.char c
    *> Attoparsec.skipSpace
    *> parser
    <?> "prefixed"

takeUntilEndOfLine :: Attoparsec.Parser Text.Text
takeUntilEndOfLine =
  Attoparsec.takeWhile1 (not . Attoparsec.isEndOfLine)
    <?> "takeUntilEndOfLine"

skipToEndOfLine :: Attoparsec.Parser ()
skipToEndOfLine = Attoparsec.skipWhile $ not . Attoparsec.isEndOfLine

----------             Parsing Blocks             ----------

parseParagraph :: Attoparsec.Parser Core.Block
parseParagraph =
  Core.Paragraph
    <$> parseInlines
    <?> "parseParagraph"

parseHeading :: Attoparsec.Parser Core.Block
parseHeading =
  Core.Heading
    <$> prefixed '#' takeUntilEndOfLine
    <?> "parseHeading"

parseBullet :: Attoparsec.Parser Core.Block
parseBullet =
  Core.Bullet
    <$> prefixed '-' parseInlines
    <?> "parseBullet"

parseQuote :: Attoparsec.Parser Core.Block
parseQuote =
  Core.Quote
    <$> prefixed '>' parseInlines
    <?> "parseQuote"

parseTag :: Attoparsec.Parser Core.Block
parseTag =
  Core.Tag
    <$> prefixed '!' word
    <?> "parseTag"

parseKeyValue :: Attoparsec.Parser Core.Block
parseKeyValue = prefixed '!' inner <?> "parseKeyValue"
  where
    inner :: Attoparsec.Parser Core.Block
    inner = do
      key <- word
      _ <- whitespace
      value <- takeUntilEndOfLine
      return $ Core.KeyValue key value

parseTriple :: Attoparsec.Parser Core.Block
parseTriple = prefixed '&' inner <?> "triple"
  where
    inner :: Attoparsec.Parser Core.Block
    inner = do
      subject <- word
      _ <- whitespace
      predicate <- word
      _ <- whitespace
      object <- takeUntilEndOfLine
      return $ Core.Triple subject predicate object

parseNonBlankBlock :: Attoparsec.Parser Core.Block
parseNonBlankBlock =
  parseHeading
    <|> parseBullet
    <|> parseQuote
    <|> parseKeyValue
    <|> parseTag
    <|> parseTriple
    <|> parseParagraph
    <?> "parseNonBlankBlock"

parseNonBlankBlocks :: Attoparsec.Parser [Core.Authored]
parseNonBlankBlocks = many1 (Core.Raw <$> parseNonBlankBlock) <?> "parseNonBlankBlocks"

----------          Parsing Blank Blocks          ----------

parseNewLines :: Attoparsec.Parser [Core.Authored]
parseNewLines =
  do
    eols <- many1 (Attoparsec.takeWhile Attoparsec.isHorizontalSpace *> Attoparsec.endOfLine)
    let len = length eols
    return $ Core.Raw <$> replicate (len - 1) Core.Blank
    <?> "parseNewLines"

----------          Parsing Transclusions         ----------

parseWholeDocument :: Attoparsec.Parser Core.TransclusionOptions
parseWholeDocument =
  do
    _ <- Attoparsec.skipSpace
    return Core.WholeDocument
    <?> "parseWholeDocument"

parseFirstLines :: Attoparsec.Parser Core.TransclusionOptions
parseFirstLines =
  Core.FirstLines
    <$> prefixed '|' Attoparsec.decimal
    <?> "parseFirstLines"

parseLines :: Attoparsec.Parser Core.TransclusionOptions
parseLines = prefixed '|' inner <?> "parseLines"
  where
    inner = do
      start <- Attoparsec.decimal
      _ <- Attoparsec.skipSpace
      length <- Attoparsec.decimal
      return $ Core.Lines start length

parseHeadingSection :: Attoparsec.Parser Core.TransclusionOptions
parseHeadingSection =
  Core.HeadingSection
    <$> prefixed '#' takeUntilEndOfLine
    <?> "parseHeadingSection"

parseTransclusionOptions :: Attoparsec.Parser Core.TransclusionOptions
parseTransclusionOptions =
  parseLines
    <|> parseFirstLines
    <|> parseHeadingSection
    <|> parseWholeDocument
    <?> "parseTransclusionOptions"

parseTransclusion :: Attoparsec.Parser Core.Transclusion
parseTransclusion = prefixed '$' inner <?> "parseTransclusion"
  where
    inner = do
      docName <- parseDocumentName
      _ <- Attoparsec.skipSpace
      options <- parseTransclusionOptions
      _ <- skipToEndOfLine
      return $ Core.Transclusion docName options

parseTransclusions :: Attoparsec.Parser [Core.Authored]
parseTransclusions = many1 (Core.ToResolve <$> parseTransclusion) <?> "parseTransclusions"

------------------------------------------------------------
--                    Document Parsing                    --
------------------------------------------------------------

parseAuthoreds :: Attoparsec.Parser [Core.Authored]
parseAuthoreds =
  concat
    <$> many1
      ( parseTransclusions
          <|> parseNonBlankBlocks
          <|> parseNewLines
      )
    <?> "parseAuthoreds"
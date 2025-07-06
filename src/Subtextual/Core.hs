module Subtextual.Core
  ( Document,
    document,
    title,
    content,
    DocumentName,
    documentName,
    unDocumentName,
    Block (..),
    Authored (..),
    authored,
    catToResolve,
    Readable (..),
    readable,
    opts,
    target,
    Inline (..),
    isSlashLinkChar,
    isAngledUrlChar,
    Transclusion (..),
    TransclusionOptions (..),
  )
where

import Data.Char (isAlpha, isDigit, isSpace)
import qualified Data.Text as T

------------------------------------------------------------
--                        Documents                       --
------------------------------------------------------------

data Document a = Document DocumentName [a]

document :: DocumentName -> [a] -> Document a
document = Document

title :: Document a -> DocumentName
title (Document t _) = t

content :: Document a -> [a]
content (Document _ c) = c

------------------------------------------------------------
--                     Document Names                     --
------------------------------------------------------------

newtype DocumentName = DocumentName {unDocumentName :: T.Text}
  deriving (Show, Eq, Ord)

documentName :: T.Text -> DocumentName
documentName = DocumentName . T.filter isSlashLinkChar

isSlashLinkChar :: Char -> Bool
isSlashLinkChar c =
  isAlpha c
    || isDigit c
    || c == '-'
    || c == '_'
    || c == '/'

------------------------------------------------------------
--                         Blocks                         --
------------------------------------------------------------

----------              Transclusion              ----------

data Transclusion
  = Transclusion DocumentName TransclusionOptions
  deriving (Show, Eq)

target :: Transclusion -> DocumentName
target (Transclusion name _) = name

opts :: Transclusion -> TransclusionOptions
opts (Transclusion _ options) = options

----------          Transclusion Options          ----------

data TransclusionOptions
  = WholeDocument
  | FirstLines Int
  | Lines Int Int
  | HeadingSection T.Text
  deriving (Show, Eq)

----------                 Blocks                 ----------

data Block
  = Paragraph [Inline]
  | Heading T.Text
  | Bullet [Inline]
  | Quote [Inline]
  | Tag T.Text
  | KeyValue T.Text T.Text
  | Triple T.Text T.Text T.Text
  | Blank
  deriving (Show, Eq)

----------          Blocks and References         ----------

data Authored
  = Raw Block
  | ToResolve Transclusion
  deriving (Show, Eq)

authored :: (Block -> a) -> (Transclusion -> a) -> Authored -> a
authored f _ (Raw x) = f x
authored _ g (ToResolve y) = g y

catToResolve :: [Authored] -> [Transclusion]
catToResolve as = [t | ToResolve t <- as]

data Readable
  = Present Block
  | TransclusionMissing DocumentName
  deriving (Show, Eq)

readable :: (Block -> a) -> (DocumentName -> a) -> Readable -> a
readable f _ (Present x) = f x
readable _ g (TransclusionMissing y) = g y

{-
 ┌───────────────────────────┐
 │ Corpus                    │
 │                           │
 │ ┌───────────────────────┐ │
 │ │ SubText File .subtext ◄─┼─────────┐
 │ └───────────┬───────────┘ │         │
 └┬────────────┼─────────────┘         │
  │            ▼                       │
  │         Parsing                Unparsing
  │            │                       ▲
  │            │                       │
  │     ┌──────▼───────┐               │
  │     │  [Authored]  ├───────────────┘
  │     └──────┬───────┘
  │            │
  │            ▼
  └──►resolveTransclusions
               │
               │
         ┌─────▼──────┐
         │ [Readable] │
         └────────────┘
-}

------------------------------------------------------------
--                     Inline Elements                    --
------------------------------------------------------------

data Inline
  = PlainText T.Text
  | BareUrl T.Text
  | AngledUrl T.Text
  | SlashLink DocumentName
  deriving (Show, Eq)

----------              Constructors              ----------

isAngledUrlChar :: Char -> Bool
isAngledUrlChar c = not $ c == '<' || c == '>' || isSpace c

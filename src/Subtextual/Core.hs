module Subtextual.Core
  ( documentName,
    isSlashLinkChar,
    DocumentName (..),
    isAngledUrlChar,
    Inline (..),
    Block (..),
    Document,
    Transclusion (..),
    TransclusionOptions (..),
    Authored(..),
    authored,
    Readable(..),
    readable,
    opts,
    target,
  )
where

import Data.Char (isAlpha, isDigit, isSpace)
import qualified Data.Text as T

newtype DocumentName = DocumentName T.Text
  deriving (Show, Eq, Ord)

isSlashLinkChar :: Char -> Bool
isSlashLinkChar c =
  isAlpha c
    || isDigit c
    || c == '-'
    || c == '_'
    || c == '/'

documentName :: T.Text -> DocumentName
documentName = DocumentName . T.filter isSlashLinkChar

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

------------------------------------------------------------
--                         Blocks                         --
------------------------------------------------------------

----------              Transclusion              ----------

data TransclusionOptions
  = WholeDocument
  | FirstLines Int
  | Lines Int Int
  | HeadingSection T.Text
  deriving (Show, Eq)

data Transclusion
  = Transclusion DocumentName TransclusionOptions
  deriving (Show, Eq)

target :: Transclusion -> DocumentName
target (Transclusion name _) = name

opts :: Transclusion -> TransclusionOptions
opts (Transclusion _ options) = options

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

type Document = [Block]

----------          Blocks and References         ----------

data Authored
  = Raw Block
  | ToResolve Transclusion 
  deriving (Show, Eq)

authored :: (Block -> a) -> (Transclusion -> a) -> Authored -> a
authored f _ (Raw x) = f x
authored _ g (ToResolve y) = g y

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
          ┌────▼─────┐
          │ Document │
          └──────────┘
-}
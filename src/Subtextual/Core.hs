module Subtextual.Core
  ( Document,
    document,
    title,
    content,
    liftD,
    DocumentName,
    documentName,
    unDocumentName,
    Block (..),
    Authored (..),
    authored,
    catToResolve,
    Resolved (..),
    resolved,
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

instance Functor Document where
  fmap f (Document name xs) = Document name $ fmap f xs

liftD :: ([a] -> [b]) -> Document a -> Document b
liftD f (Document name xs) = Document name $ f xs

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
--                    Lines in Document                   --
------------------------------------------------------------

----------             Content Blocks             ----------

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

data Resolved
  = Present Block
  | TransclusionMissing DocumentName
  | HeadingMissing DocumentName T.Text
  deriving (Show, Eq)

resolved ::
  (Block -> a) ->
  (DocumentName -> a) ->
  (DocumentName -> T.Text -> a) ->
  Resolved ->
  a
resolved f _ _ (Present block) = f block
resolved _ g _ (TransclusionMissing docName) = g docName
resolved _ _ h (HeadingMissing docName headingName) = h docName headingName
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
         │ [Resolved] │
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

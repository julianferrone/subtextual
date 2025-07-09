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
    catToResolves,
    Resolved (..),
    resolved,
    resolveAuthored,
    resolveWithoutLookup,
    opts,
    target,
    Inline (..),
    isSlashLinkChar,
    isAngledUrlChar,
    Transclusion (..),
    TransclusionOptions (..),
  )
where

import qualified Data.Char as Char
import Data.List
import qualified Data.Text as Text

------------------------------------------------------------
--                        Documents                       --
------------------------------------------------------------

data Document a = Document DocumentName [a] deriving (Eq, Show)

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

newtype DocumentName = DocumentName {unDocumentName :: Text.Text}
  deriving (Show, Eq, Ord)

documentName :: Text.Text -> DocumentName
documentName = DocumentName . Text.filter isSlashLinkChar

isSlashLinkChar :: Char -> Bool
isSlashLinkChar c =
  Char.isAlpha c
    || Char.isDigit c
    || c == '-'
    || c == '_'
    || c == '/'

------------------------------------------------------------
--                    Lines in Document                   --
------------------------------------------------------------

----------             Content Blocks             ----------

data Block
  = Paragraph [Inline]
  | Heading Text.Text
  | Bullet [Inline]
  | Quote [Inline]
  | Tag Text.Text
  | KeyValue Text.Text Text.Text
  | Triple Text.Text Text.Text Text.Text
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
  | HeadingSection Text.Text
  deriving (Show, Eq)

----------          Blocks and References         ----------

data Authored
  = Raw Block
  | ToResolve Transclusion
  deriving (Show, Eq)

isRaw :: Authored -> Bool
isRaw (Raw _) = True
isRaw _ = False

authored :: (Block -> a) -> (Transclusion -> a) -> Authored -> a
authored f _ (Raw x) = f x
authored _ g (ToResolve y) = g y

catRaws :: [Authored] -> [Block]
catRaws as = [b | Raw b <- as]

catToResolves :: [Authored] -> [Transclusion]
catToResolves as = [t | ToResolve t <- as]

data Resolved
  = Present Block
  | ResourceNotFound DocumentName
  | HeadingNotFound DocumentName Text.Text
  deriving (Show, Eq)

resolved ::
  (Block -> a) ->
  (DocumentName -> a) ->
  (DocumentName -> Text.Text -> a) ->
  Resolved ->
  a
resolved f _ _ (Present block) = f block
resolved _ g _ (ResourceNotFound docName) = g docName
resolved _ _ h (HeadingNotFound docName headingName) = h docName headingName

resolveAuthored ::
  (Transclusion -> [Resolved]) -> -- Function to lookup transclusion references
  [Authored] -> -- Document content that needs resolution
  [Resolved] -- Resolved Document content
resolveAuthored f as =
  if all isRaw as
    then fmap Present . catRaws $ as
    else mconcat . fmap (authored (singleton . Present) f) $ as

-- This is a function to convert all transclusion references to "ResourceNotFound".
-- The reason why we do this is so that we can 
resolveWithoutLookup :: [Authored] -> [Resolved]
resolveWithoutLookup = fmap resolve where
  resolve (Raw b) = Present b
  resolve (ToResolve t) = ResourceNotFound . target $ t

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
  └─► resolveTransclusions
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
  = PlainText Text.Text
  | BareUrl Text.Text
  | AngledUrl Text.Text
  | SlashLink DocumentName
  deriving (Show, Eq)

----------              Constructors              ----------

isAngledUrlChar :: Char -> Bool
isAngledUrlChar c = not $ c == '<' || c == '>' || Char.isSpace c

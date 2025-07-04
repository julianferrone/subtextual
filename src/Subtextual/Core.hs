module Subtextual.Core
  ( documentName,
    isSlashLinkChar,
    DocumentName (..),
    isAngledUrlChar,
    Inline (..),
    Block (..),
    Document,
  )
where

import Data.Char (isAlpha, isDigit, isSpace)
import qualified Data.Map as Map
import qualified Data.Text as T

newtype DocumentName = DocumentName T.Text
  deriving (Show, Eq)

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

------------------------------------------------------------
--                   Corpus of Documents                  --
------------------------------------------------------------

data Corpus
  = Corpus
      (Map DocumentName Document) -- The map of document names to documents
      (Map DocumentName (Map String Document)) -- Map of document names to map of heading names to subsections

resolveTransclusion :: Corpus -> Transclusion -> [Block]
resolveTransclusion (Corpus docs _) (Transclusion name WholeDocument) = Map.lookup name

resolveTransclusions :: Corpus -> [Either Block Transclusion] -> [Block]
resolveTransclusions = fmap . either id . resolveTransclusion
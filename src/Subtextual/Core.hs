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
    BlockOrRef,
    opts,
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

type BlockOrRef = Either Block Transclusion


module Subtextual.Core
  ( documentName,
    isSlashLinkChar,
    DocumentName(..),
    isAngledUrlChar,
    Inline(..),
    AuthoredBlock (..),
    AuthoredDocument,
  )
where

import Data.Char (isAlpha, isDigit, isSpace)
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

{-
TODO: Specialise the `AuthoredBlock` type into `AuthoredBlock` and add a `ViewerBlock`
which represents the block after we fill out the transclusions in the document
from the corpus.

TODO: Add a `Corpus` type to represent a collection of `AuthoredDocument`s.
-}

----------             Authored Blocks            ----------

data AuthoredBlock
  = AParagraph [Inline]
  | AHeading T.Text
  | ABullet [Inline]
  | AQuote [Inline]
  | ATag T.Text
  | AKeyValue T.Text T.Text
  | ATriple T.Text T.Text T.Text
  | ATransclusion TransclusionOptions
  | ABlank
  deriving (Show, Eq)

type AuthoredDocument = [AuthoredBlock]
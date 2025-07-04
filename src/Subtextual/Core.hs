module Subtextual.Core
  ( Inline (..),
    AuthoredBlock (..),
    AuthoredDocument,
  )
where

import qualified Data.Text as T

data Inline
  = PlainText T.Text
  | BareUrl T.Text
  | AngledUrl T.Text
  | SlashLink T.Text
  deriving (Show, Eq)

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
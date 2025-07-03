module Subtextual.Core
    ( Inline(..), Block(..), Document
    ) where

import qualified Data.Text as T

data Inline =
    PlainText T.Text
    | BareUrl T.Text
    | AngledUrl T.Text
    | SlashLink T.Text
    deriving (Show, Eq)

data TransclusionOptions =
    WholeDocument
    | FirstLines Int
    | Lines Int Int
    | HeadingSection T.Text
    deriving (Show, Eq)

{-
TODO: Specialise the `Block` type into `AuthoredBlock` and add a `ViewerBlock`
which represents the block after we fill out the transclusions in the document
from the corpus.

TODO: Add a `Corpus` type to represent a collection of `Document`s.
-}

data Block = 
    Paragraph [Inline]
    | Heading T.Text
    | Bullet [Inline]
    | Quote [Inline]
    | Tag T.Text
    | KeyValue T.Text T.Text
    | Triple T.Text T.Text T.Text
    | Transclusion TransclusionOptions
    | Blank
    deriving (Show, Eq)

type Document = [Block]
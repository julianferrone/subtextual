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

data Block = 
    Paragraph [Inline]
    | Heading T.Text
    | Bullet [Inline]
    | Quote [Inline]
    | Tag T.Text
    | KeyValue T.Text T.Text
    | Triple T.Text T.Text T.Text
    | Blank
    deriving (Show, Eq)

type Document = [Block]
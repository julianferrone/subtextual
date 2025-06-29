module Subtext
    ( Inline(..), Block(..), Document(..)
    ) where

import qualified Data.Text as T

data Inline =
    PlainText T.Text
    | BareUrl T.Text
    | AngledUrl T.Text
    deriving (Show, Eq)

data Block = 
    Paragraph [Inline]
    | Heading T.Text
    | Bullet [Inline]
    | Quote [Inline]
    | Blank
    deriving (Show, Eq)

type Document = [Block]
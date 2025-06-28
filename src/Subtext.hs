module Subtext
    ( Inline, Block, Document
    ) where

import qualified Data.Text as T

data Inline =
    PlainText T.Text
    | BareUrl T.Text
    | AngledUrl T.Text


data Block = 
    Paragraph [Inline]
    | Heading T.Text
    | List [Inline]
    | Quote [Inline]
    | Blank

type Document = [Block]
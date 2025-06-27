module Subtext
    ( Inline, Block, Document
    ) where

data Inline =
    PlainText Text
    | BareUrl Text
    | AngledUrl Text


data Block = 
    Paragraph [Inline]
    | Heading Text
    | List [Inline]
    | Quote [Inline]
    | Blank

type Document = [Block]
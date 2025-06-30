module Subtextual.Unparser
    (inline, block, document) where

import Subtextual.Core
import qualified Data.Text as T

inline :: Inline -> T.Text
inline (PlainText p) = p
inline (BareUrl url) = url
inline (AngledUrl url) = T.pack "<" <> url <> T.pack ">"
inline (SlashLink sl) = T.pack "/" <> sl

block :: Block -> T.Text
block (Paragraph p) = (mconcat . map inline) p
block (Heading h) = T.pack "# " <> h
block (Bullet b) = T.pack "- " <> (mconcat . map inline) b
block (Quote q) = T.pack "> " <> (mconcat . map inline) q
block Blank = T.pack ""

document :: Document -> T.Text
document = T.intercalate (T.pack "\n") . map block
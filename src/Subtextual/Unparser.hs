module Subtextual.Unparser
    (inline, block, document) where

import Subtextual.Core
import qualified Data.Text as T

inline :: Inline -> T.Text
inline (PlainText p) = p
inline (BareUrl url) = url
inline (AngledUrl url) = T.pack "<" <> url <> T.pack ">"
inline (SlashLink sl) = T.pack "/" <> sl

inlines :: [Inline] -> T.Text
inlines = mconcat . map inline

spaced :: [T.Text] -> T.Text
spaced = T.intercalate $ T.pack " "

block :: Block -> T.Text
block (Paragraph paragraph) = inlines paragraph
block (Heading heading) = T.pack "# " <> heading
block (Bullet bullet) = T.pack "- " <> inlines bullet
block (Quote quote) = T.pack "> " <> inlines quote
block Blank = T.pack ""
block (Tag tag) = T.pack "! " <> tag
block (KeyValue key value) = spaced [T.pack "!", key, value]
block (Triple subject predicate object) = spaced [T.pack "&", subject, predicate, object]

document :: Document -> T.Text
document = T.intercalate (T.pack "\n") . map block
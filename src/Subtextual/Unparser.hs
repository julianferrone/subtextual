module Subtextual.Unparser (inline, block, document) where

import qualified Data.Text as T
import Subtextual.Core

inline :: Inline -> T.Text
inline (PlainText p) = p
inline (BareUrl url) = url
inline (AngledUrl url) = T.pack "<" <> url <> T.pack ">"
inline (SlashLink (DocumentName dn)) = T.pack "/" <> dn

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
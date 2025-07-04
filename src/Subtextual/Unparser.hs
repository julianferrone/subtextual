module Subtextual.Unparser (inline, block, document) where

import qualified Data.Text as T
import Subtextual.Core

inline :: Inline -> T.Text
inline (PlainText p) = p
inline (BareUrl url) = url
inline (AngledUrl url) = T.pack "<" <> url <> T.pack ">"
inline (SlashLink sl) = T.pack "/" <> sl

inlines :: [Inline] -> T.Text
inlines = mconcat . map inline

spaced :: [T.Text] -> T.Text
spaced = T.intercalate $ T.pack " "

block :: AuthoredBlock -> T.Text
block (AParagraph paragraph) = inlines paragraph
block (AHeading heading) = T.pack "# " <> heading
block (ABullet bullet) = T.pack "- " <> inlines bullet
block (AQuote quote) = T.pack "> " <> inlines quote
block ABlank = T.pack ""
block (ATag tag) = T.pack "! " <> tag
block (AKeyValue key value) = spaced [T.pack "!", key, value]
block (ATriple subject predicate object) = spaced [T.pack "&", subject, predicate, object]

document :: AuthoredDocument -> T.Text
document = T.intercalate (T.pack "\n") . map block
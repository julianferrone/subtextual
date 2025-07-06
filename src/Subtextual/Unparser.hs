module Subtextual.Unparser (unparseInline, unparseBlock, unparseAuthoreds) where

import Data.Maybe
import qualified Data.Text as T
import Subtextual.Core

unparseInline :: Inline -> T.Text
unparseInline (PlainText p) = p
unparseInline (BareUrl url) = url
unparseInline (AngledUrl url) = T.pack "<" <> url <> T.pack ">"
unparseInline (SlashLink docName) = T.pack "/" <> unDocumentName docName

unparseInlines :: [Inline] -> T.Text
unparseInlines = mconcat . map unparseInline

spaced :: [T.Text] -> T.Text
spaced = T.intercalate $ T.pack " "

----------            Unparsing Blocks            ----------

unparseBlock :: Block -> T.Text
unparseBlock (Paragraph paragraph) = unparseInlines paragraph
unparseBlock (Heading heading) = T.pack "# " <> heading
unparseBlock (Bullet bullet) = T.pack "- " <> unparseInlines bullet
unparseBlock (Quote quote) = T.pack "> " <> unparseInlines quote
unparseBlock Blank = T.pack ""
unparseBlock (Tag tag) = T.pack "! " <> tag
unparseBlock (KeyValue key value) = spaced [T.pack "!", key, value]
unparseBlock (Triple subject predicate object) = spaced [T.pack "&", subject, predicate, object]

----------         Unparsing Transclusions        ----------

unparseTransclusionOptions :: TransclusionOptions -> [T.Text]
unparseTransclusionOptions WholeDocument = []
unparseTransclusionOptions (FirstLines len) =
  [ T.pack "|",
    (T.pack . show) len
  ]
unparseTransclusionOptions (Lines start len) =
  [ T.pack "|",
    (T.pack . show) start,
    (T.pack . show) len
  ]
unparseTransclusionOptions (HeadingSection name) =
  [ T.pack "#",
    name
  ]

unparseTransclusion :: Transclusion -> T.Text
unparseTransclusion (Transclusion name options) =
  spaced $
    [ T.pack "$",
      unDocumentName name
    ]
      <> unparseTransclusionOptions options

unparseAuthored :: Authored -> T.Text
unparseAuthored = authored unparseBlock unparseTransclusion

----------          Unparsing Collections         ----------

unparseAuthoreds :: [Authored] -> T.Text
unparseAuthoreds = T.intercalate (T.pack "\n") . map unparseAuthored
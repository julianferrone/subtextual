module Subtextual.Unparser (unparseInline, unparseBlock, unparseAuthoreds) where

import Data.Maybe
import qualified Data.Text as Text
import qualified Subtextual.Core as Core

unparseInline :: Core.Inline -> Text.Text
unparseInline (Core.PlainText p) = p
unparseInline (Core.BareUrl url) = url
unparseInline (Core.AngledUrl url) = Text.pack "<" <> url <> Text.pack ">"
unparseInline (Core.SlashLink docName) = Text.pack "/" <> Core.unDocumentName docName

unparseInlines :: [Core.Inline] -> Text.Text
unparseInlines = mconcat . map unparseInline

spaced :: [Text.Text] -> Text.Text
spaced = Text.intercalate $ Text.pack " "

----------            Unparsing Blocks            ----------

unparseBlock :: Core.Block -> Text.Text
unparseBlock (Core.Paragraph paragraph) = unparseInlines paragraph
unparseBlock (Core.Heading heading) = Text.pack "# " <> heading
unparseBlock (Core.Bullet bullet) = Text.pack "- " <> unparseInlines bullet
unparseBlock (Core.Quote quote) = Text.pack "> " <> unparseInlines quote
unparseBlock Core.Blank = Text.pack ""
unparseBlock (Core.Tag tag) = Text.pack "! " <> tag
unparseBlock (Core.KeyValue key value) = spaced [Text.pack "!", key, value]
unparseBlock (Core.Triple subject predicate object) = spaced [Text.pack "&", subject, predicate, object]

----------         Unparsing Transclusions        ----------

unparseTransclusionOptions :: Core.TransclusionOptions -> [Text.Text]
unparseTransclusionOptions Core.WholeDocument = []
unparseTransclusionOptions (Core.FirstLines len) =
  [ Text.pack "|",
    (Text.pack . show) len
  ]
unparseTransclusionOptions (Core.Lines start len) =
  [ Text.pack "|",
    (Text.pack . show) start,
    (Text.pack . show) len
  ]
unparseTransclusionOptions (Core.HeadingSection name) =
  [ Text.pack "#",
    name
  ]

unparseTransclusion :: Core.Transclusion -> Text.Text
unparseTransclusion (Core.Transclusion name options) =
  spaced $
    [ Text.pack "$",
      Core.unDocumentName name
    ]
      <> unparseTransclusionOptions options

unparseAuthored :: Core.Authored -> Text.Text
unparseAuthored = Core.authored unparseBlock unparseTransclusion

----------          Unparsing Collections         ----------

unparseAuthoreds :: [Core.Authored] -> Text.Text
unparseAuthoreds = Text.intercalate (Text.pack "\n") . map unparseAuthored
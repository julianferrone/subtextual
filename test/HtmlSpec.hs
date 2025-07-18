module HtmlSpec (spec) where

import qualified Data.Text as T
import Lucid
import Subtextual.Core
import qualified Subtextual.Html as Html
import Test.Hspec

testDocument :: Document Resolved
testDocument =
  document
    ((documentName . T.pack) "test")
    $ Present
      <$> [ Heading (T.pack "Overview"),
            Blank,
            Paragraph [PlainText (T.pack "Evolution is a behavior that emerges in any system with:")],
            Blank,
            Bullet [PlainText (T.pack "Mutation")],
            Bullet [PlainText (T.pack "Heredity")],
            Bullet [PlainText (T.pack "Selection")],
            Blank,
            Paragraph [PlainText (T.pack "Evolutionary systems often generate unexpected solutions. Nature selects for good enough.")],
            Blank,
            Quote
              [ PlainText (T.pack "There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you’re living in. (Olivia Judson, Santa Fe Institute "),
                BareUrl (T.pack "https://overcast.fm/+UtNTAcN2Y/13:36"),
                PlainText (T.pack " )")
              ],
            Blank,
            Paragraph
              [ PlainText (T.pack "Evolving systems exist in "),
                (SlashLink . documentName . T.pack) "punctuated-equilibrium",
                PlainText (T.pack ".")
              ],
            Blank,
            Heading (T.pack "Questions"),
            Blank,
            Bullet [PlainText (T.pack "What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.")],
            Bullet [PlainText (T.pack "What happens to an evolutionary system when you remove mutation? Heredity? Selection?")],
            Bullet [PlainText (T.pack "Do you see a system with one of these properties? How can you introduce the other two?")],
            Blank,
            Blank,
            Heading (T.pack "See also"),
            Blank,
            Paragraph [BareUrl (T.pack "https://en.wikipedia.org/wiki/Evolutionary_systems")]
          ]

spec :: Spec
spec = do
  describe "block" $ do
    it "renders a plaintext paragraph into HTML" $ do
      Html.renderBlock (Paragraph [PlainText (T.pack "Hello, world!")])
        `shouldBe` T.pack "<p><span>Hello, world!</span></p>"
    it "renders a complex paragraph into HTML" $ do
      Html.renderBlock
        ( Paragraph
            [ PlainText (T.pack "Hello, "),
              BareUrl (T.pack "https://google.com"),
              PlainText (T.pack " and "),
              AngledUrl (T.pack "doi:10.1000/100")
            ]
        )
        `shouldBe` T.pack "<p><span>Hello, </span><a href=\"https://google.com\">https://google.com</a><span> and </span><a href=\"doi:10.1000/100\">doi:10.1000/100</a></p>"
    it "renders a heading into HTML" $ do
      Html.renderBlock (Heading (T.pack "Test"))
        `shouldBe` T.pack "<h2>Test</h2>"
    it "renders a bullet into HTML" $ do
      Html.renderBlock (Bullet [PlainText (T.pack "Test")])
        `shouldBe` T.pack "<li><span>Test</span></li>"
    it "renders a quote into HTML" $ do
      Html.renderBlock (Quote [PlainText (T.pack "Test")])
        `shouldBe` T.pack "<blockquote><span>Test</span></blockquote>"
    it "renders a slashlink into HTML" $ do
      Html.renderBlock (Paragraph [(SlashLink . documentName . T.pack) "test"])
        `shouldBe` T.pack "<p><a href=\"test\" class=\"slashlink\">test</a></p>"
    it "renders a tag into HTML" $ do
      Html.renderBlock (Tag (T.pack "Tag"))
        `shouldBe` T.pack "<div class=\"tag\">Tag</div>"
    it "renders a key value pair into HTML" $ do
      Html.renderBlock (KeyValue (T.pack "Key") (T.pack "Value"))
        `shouldBe` T.pack "<div class=\"keyvalue\"><div class=\"key\">Key</div><div class=\"value\">Value</div></div>"
    it "renders a triple into HTML" $ do
      Html.renderBlock (Triple (T.pack "Subject") (T.pack "Predicate") (T.pack "Object"))
        `shouldBe` T.pack "<div class=\"triple\"><div class=\"subject\">Subject</div><div class=\"predicate\">Predicate</div><div class=\"object\">Object</div></div>"
  describe "document" $ do
    it "renders a document into HTML" $ do
      Html.renderDoc testDocument
        `shouldBe` T.pack "<h2>Overview</h2><p><span>Evolution is a behavior that emerges in any system with:</span></p><ul><li><span>Mutation</span></li><li><span>Heredity</span></li><li><span>Selection</span></li></ul><p><span>Evolutionary systems often generate unexpected solutions. Nature selects for good enough.</span></p><blockquote><span>There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you\8217re living in. (Olivia Judson, Santa Fe Institute </span><a href=\"https://overcast.fm/+UtNTAcN2Y/13:36\">https://overcast.fm/+UtNTAcN2Y/13:36</a><span> )</span></blockquote><p><span>Evolving systems exist in </span><a href=\"punctuated-equilibrium\" class=\"slashlink\">punctuated-equilibrium</a><span>.</span></p><h2>Questions</h2><ul><li><span>What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.</span></li><li><span>What happens to an evolutionary system when you remove mutation? Heredity? Selection?</span></li><li><span>Do you see a system with one of these properties? How can you introduce the other two?</span></li></ul><h2>See also</h2><p><a href=\"https://en.wikipedia.org/wiki/Evolutionary_systems\">https://en.wikipedia.org/wiki/Evolutionary_systems</a></p>"

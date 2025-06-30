module HtmlSpec (spec) where

import Subtextual.Core
import qualified Subtextual.Html as Html

import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Lucid

import Test.Hspec

renderBlock :: Block -> T.Text
renderBlock = toStrict . renderText . Html.block

renderDoc :: Document -> T.Text
renderDoc = toStrict . renderText . Html.document

spec :: Spec
spec = do
    describe "block" $ do
        it "renders a plaintext paragraph into HTML" $ do
            renderBlock (Paragraph [PlainText (T.pack "Hello, world!")]) 
                `shouldBe` T.pack "<p><span>Hello, world!</span></p>"
        it "renders a complex paragraph into HTML" $ do
            renderBlock (Paragraph 
                [PlainText (T.pack "Hello, ")
                , BareUrl (T.pack "https://google.com")
                , PlainText (T.pack " and ")
                , AngledUrl (T.pack "doi:10.1000/100")
                ]) 
                `shouldBe` T.pack "<p><span>Hello, </span><a href=\"https://google.com\">https://google.com</a><span> and </span><a href=\"doi:10.1000/100\">doi:10.1000/100</a></p>"
        it "renders a heading into HTML" $ do
            renderBlock (Heading (T.pack "Test"))
                `shouldBe` T.pack "<h2>Test</h2>"
        it "renders a bullet into HTML" $ do
            renderBlock (Bullet [PlainText (T.pack "Test")])
                `shouldBe` T.pack "<li><span>Test</span></li>"
        it "renders a quote into HTML" $ do
            renderBlock (Quote [PlainText (T.pack "Test")])
                `shouldBe` T.pack "<blockquote><span>Test</span></blockquote>"
    describe "document" $ do
        it "renders a document into HTML" $ do
            renderDoc [
                    Heading (T.pack "Overview"),
                    Blank,
                    Paragraph [PlainText (T.pack "Evolution is a behavior that emerges in any system with:")],
                    Blank,
                    Bullet [PlainText (T.pack "Mutation")],
                    Bullet [PlainText (T.pack "Heredity")],
                    Bullet [PlainText (T.pack "Selection")],
                    Blank,
                    Paragraph [PlainText (T.pack "Evolutionary systems often generate unexpected solutions. Nature selects for good enough.")],
                    Blank,
                    Quote [
                        PlainText (T.pack "There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you’re living in. (Olivia Judson, Santa Fe Institute "),
                        BareUrl (T.pack "https://overcast.fm/+UtNTAcN2Y/13:36"),
                        PlainText (T.pack " )")
                    ],
                    Blank,
                    Paragraph [PlainText (T.pack "Evolving systems exist in /punctuated-equilibrium.")],
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
                ] `shouldBe` T.pack "<h2>Overview</h2><p><span>Evolution is a behavior that emerges in any system with:</span></p><ul><li><span>Mutation</span></li><li><span>Heredity</span></li><li><span>Selection</span></li></ul><p><span>Evolutionary systems often generate unexpected solutions. Nature selects for good enough.</span></p><blockquote><span>There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you\8217re living in. (Olivia Judson, Santa Fe Institute </span><a href=\"https://overcast.fm/+UtNTAcN2Y/13:36\">https://overcast.fm/+UtNTAcN2Y/13:36</a><span> )</span></blockquote><p><span>Evolving systems exist in /punctuated-equilibrium.</span></p><h2>Questions</h2><ul><li><span>What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.</span></li><li><span>What happens to an evolutionary system when you remove mutation? Heredity? Selection?</span></li><li><span>Do you see a system with one of these properties? How can you introduce the other two?</span></li></ul><h2>See also</h2><p><a href=\"https://en.wikipedia.org/wiki/Evolutionary_systems\">https://en.wikipedia.org/wiki/Evolutionary_systems</a></p>"

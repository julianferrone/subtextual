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
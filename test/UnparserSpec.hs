module UnparserSpec (spec) where

import Subtextual.Core
import qualified Subtextual.Unparser as Unparser

import qualified Data.Text as T
import Data.Attoparsec.Text

import Test.Hspec

spec :: Spec
spec = do
    describe "inline" $ do
        it "unparses a plaintext" $ do
            Unparser.inline (PlainText (T.pack "Hello, world!"))
                `shouldBe` T.pack "Hello, world!"
        it "unparses a bare URL" $ do
            Unparser.inline (BareUrl (T.pack "https://google.com"))
                `shouldBe` T.pack "https://google.com"
        it "unparses an angle-delimited URL" $ do
            Unparser.inline (AngledUrl (T.pack "doi:10.1000/100"))
                `shouldBe` T.pack "<doi:10.1000/100>"
        it "unparses a slashlink" $ do
            Unparser.inline (SlashLink (T.pack "slash-link"))
                `shouldBe` T.pack "/slash-link"
    describe "block" $ do
        it "unparses a paragraph" $ do
            Unparser.block (Paragraph [
                    PlainText (T.pack "This is a HTTP URL: "),
                    BareUrl (T.pack "https://www.google.com"),
                    PlainText (T.pack ", and this is an angle URL: "),
                    AngledUrl (T.pack "doi:10.1000/100"),
                    PlainText (T.pack ", and this is a slashlink: "),
                    SlashLink (T.pack "slash-link")
                ]) `shouldBe` T.pack "This is a HTTP URL: https://www.google.com, and this is an angle URL: <doi:10.1000/100>, and this is a slashlink: /slash-link"
        it "unparses a heading" $ do
            Unparser.block (Heading (T.pack "Heading"))
                `shouldBe` T.pack "# Heading"
        it "unparses a bullet" $ do
            Unparser.block 
                (Bullet [
                    PlainText (T.pack "Here's another slashlink: "), 
                    SlashLink (T.pack "electric/boogaloo")
                ])
                `shouldBe` T.pack "- Here's another slashlink: /electric/boogaloo"
        it "unparses a quote" $ do
            Unparser.block (Quote [PlainText (T.pack "Here's a quote")])
                `shouldBe` T.pack "> Here's a quote"
        it "unparses a blank line" $ do
            Unparser.block Blank `shouldBe` T.pack ""
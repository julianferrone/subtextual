module ParserSpec (spec) where

import Subtextual.Core
import qualified Subtextual.Parser as Parser

import qualified Data.Text as T
import Data.Attoparsec.Text

import Test.Hspec

shouldMatch :: (Show a, Eq a) => Parser a -> T.Text -> a -> IO ()
shouldMatch parser input result = parseOnly parser input `shouldBe` Right result

spec :: Spec
spec = do
    describe "nonBlankBlock" $ do
        it "parses a paragraph" $ do
            shouldMatch 
                Parser.nonBlankBlock 
                (T.pack "Hello, world!")
                (Paragraph [PlainText (T.pack "Hello, world!")])
        it "parses a HTTP URL" $ do
            shouldMatch
                Parser.nonBlankBlock
                (T.pack "http://www.google.com")
                (Paragraph [BareUrl (T.pack "http://www.google.com")])      
        it "parses a HTTPS URL" $ do
            shouldMatch
                Parser.nonBlankBlock
                (T.pack "https://www.google.com")
                (Paragraph [BareUrl (T.pack "https://www.google.com")])         
        it "parses HTTP and angle-delimited URLs" $ do
            shouldMatch
                Parser.nonBlankBlock
                (T.pack "This is a HTTP URL: https://www.google.com, and this is an angle URL: <doi:10.1000/100>")
                (Paragraph [
                    PlainText (T.pack "This is a HTTP URL: "),
                    BareUrl (T.pack "https://www.google.com"),
                    PlainText (T.pack ", and this is an angle URL: "),
                    AngledUrl (T.pack "doi:10.1000/100")
                ])
        it "parses a heading" $ do
            shouldMatch
                Parser.nonBlankBlock
                (T.pack "# Header")
                (Heading (T.pack "Header"))
        it "parses a bullet" $ do
            shouldMatch
                Parser.nonBlankBlock
                (T.pack "- Bullet")
                (Bullet [PlainText (T.pack "Bullet")])
        it "parses a quote" $ do
            shouldMatch
                Parser.nonBlankBlock
                (T.pack "> Quote")
                (Quote [PlainText (T.pack "Quote")])
        it "parses a slashlink" $ do
            shouldMatch
                Parser.nonBlankBlock
                (T.pack "/foo/bar") 
                (Paragraph [SlashLink (T.pack "foo/bar")])
    describe "document" $ do
        it "parses a whole document" $ do
            shouldMatch
                Parser.document
                (T.pack "# Overview\n\nEvolution is a behavior that emerges in any system with:\n\n- Mutation\n- Heredity\n- Selection\n\nEvolutionary systems often generate unexpected solutions. Nature selects for good enough.\n\n> There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you\8217re living in. (Olivia Judson, Santa Fe Institute https://overcast.fm/+UtNTAcN2Y/13:36 )\n\nEvolving systems exist in /punctuated-equilibrium.\n\n# Questions\n\n- What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.\n- What happens to an evolutionary system when you remove mutation? Heredity? Selection?\n- Do you see a system with one of these properties? How can you introduce the other two?\n\n\n# See also\n\nhttps://en.wikipedia.org/wiki/Evolutionary_systems\n")
                [
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
                    Paragraph [
                        PlainText (T.pack "Evolving systems exist in "),
                        SlashLink (T.pack "punctuated-equilibrium"),
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
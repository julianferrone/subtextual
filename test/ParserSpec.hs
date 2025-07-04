module ParserSpec (spec) where

import Data.Attoparsec.Text
import qualified Data.Text as T
import Subtextual.Core
import qualified Subtextual.Parser as Parser
import Test.Hspec

shouldMatch :: (Show a, Eq a) => Parser a -> T.Text -> a -> IO ()
shouldMatch parser input result = parseOnly parser input `shouldBe` Right result

spec :: Spec
spec = do
  describe "nonABlankABlock" $ do
    it "parses a paragraph" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "Hello, world!")
        (AParagraph [PlainText (T.pack "Hello, world!")])
    it "parses a HTTP URL" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "http://www.google.com")
        (AParagraph [BareUrl (T.pack "http://www.google.com")])
    it "parses a HTTPS URL" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "https://www.google.com")
        (AParagraph [BareUrl (T.pack "https://www.google.com")])
    it "parses HTTP and angle-delimited URLs" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "This is a HTTP URL: https://www.google.com, and this is an angle URL: <doi:10.1000/100>")
        ( AParagraph
            [ PlainText (T.pack "This is a HTTP URL: "),
              BareUrl (T.pack "https://www.google.com"),
              PlainText (T.pack ", and this is an angle URL: "),
              AngledUrl (T.pack "doi:10.1000/100")
            ]
        )
    it "parses a heading" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "# Header")
        (AHeading (T.pack "Header"))
    it "parses a bullet" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "- ABullet")
        (ABullet [PlainText (T.pack "ABullet")])
    it "parses a quote" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "> AQuote")
        (AQuote [PlainText (T.pack "AQuote")])
    it "parses a slashlink" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "/foo/bar")
        (AParagraph [SlashLink (T.pack "foo/bar")])
    it "parses a tag" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "! ATag")
        (ATag (T.pack "ATag"))
    it "parses a key value pair" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "! Key Value")
        (AKeyValue (T.pack "Key") (T.pack "Value"))
    it "parses a key value pair with spaces in the value" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "! Key Value 2")
        (AKeyValue (T.pack "Key") (T.pack "Value 2"))
    it "parses a subject object predicate" $ do
      shouldMatch
        Parser.nonABlankABlock
        (T.pack "& Subject Predicate Object")
        (ATriple (T.pack "Subject") (T.pack "Predicate") (T.pack "Object"))
  describe "document" $ do
    it "parses a whole document" $ do
      shouldMatch
        Parser.document
        (T.pack "# Overview\n\nEvolution is a behavior that emerges in any system with:\n\n- Mutation\n- Heredity\n- Selection\n\nEvolutionary systems often generate unexpected solutions. Nature selects for good enough.\n\n> There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you\8217re living in. (Olivia Judson, Santa Fe Institute https://overcast.fm/+UtNTAcN2Y/13:36 )\n\nEvolving systems exist in /punctuated-equilibrium.\n\n# Questions\n\n- What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.\n- What happens to an evolutionary system when you remove mutation? Heredity? Selection?\n- Do you see a system with one of these properties? How can you introduce the other two?\n\n\n# See also\n\nhttps://en.wikipedia.org/wiki/Evolutionary_systems")
        [ AHeading (T.pack "Overview"),
          ABlank,
          AParagraph [PlainText (T.pack "Evolution is a behavior that emerges in any system with:")],
          ABlank,
          ABullet [PlainText (T.pack "Mutation")],
          ABullet [PlainText (T.pack "Heredity")],
          ABullet [PlainText (T.pack "Selection")],
          ABlank,
          AParagraph [PlainText (T.pack "Evolutionary systems often generate unexpected solutions. Nature selects for good enough.")],
          ABlank,
          AQuote
            [ PlainText (T.pack "There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you’re living in. (Olivia Judson, Santa Fe Institute "),
              BareUrl (T.pack "https://overcast.fm/+UtNTAcN2Y/13:36"),
              PlainText (T.pack " )")
            ],
          ABlank,
          AParagraph
            [ PlainText (T.pack "Evolving systems exist in "),
              SlashLink (T.pack "punctuated-equilibrium"),
              PlainText (T.pack ".")
            ],
          ABlank,
          AHeading (T.pack "Questions"),
          ABlank,
          ABullet [PlainText (T.pack "What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.")],
          ABullet [PlainText (T.pack "What happens to an evolutionary system when you remove mutation? Heredity? Selection?")],
          ABullet [PlainText (T.pack "Do you see a system with one of these properties? How can you introduce the other two?")],
          ABlank,
          ABlank,
          AHeading (T.pack "See also"),
          ABlank,
          AParagraph [BareUrl (T.pack "https://en.wikipedia.org/wiki/Evolutionary_systems")]
        ]
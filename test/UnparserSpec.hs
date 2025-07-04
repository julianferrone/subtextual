module UnparserSpec (spec) where

import Data.Attoparsec.Text
import qualified Data.Text as T
import Subtextual.Core
import qualified Subtextual.Unparser as Unparser
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
      Unparser.inline ((SlashLink . documentName . T.pack) "slash-link")
        `shouldBe` T.pack "/slash-link"
  describe "block" $ do
    it "unparses a paragraph" $ do
      Unparser.block
        ( AParagraph
            [ PlainText (T.pack "This is a HTTP URL: "),
              BareUrl (T.pack "https://www.google.com"),
              PlainText (T.pack ", and this is an angle URL: "),
              AngledUrl (T.pack "doi:10.1000/100"),
              PlainText (T.pack ", and this is a slashlink: "),
              (SlashLink . documentName . T.pack) "slash-link"
            ]
        )
        `shouldBe` T.pack "This is a HTTP URL: https://www.google.com, and this is an angle URL: <doi:10.1000/100>, and this is a slashlink: /slash-link"
    it "unparses a heading" $ do
      Unparser.block (AHeading (T.pack "AHeading"))
        `shouldBe` T.pack "# AHeading"
    it "unparses a bullet" $ do
      Unparser.block
        ( ABullet
            [ PlainText (T.pack "Here's another slashlink: "),
              (SlashLink . documentName . T.pack) "electric/boogaloo"
            ]
        )
        `shouldBe` T.pack "- Here's another slashlink: /electric/boogaloo"
    it "unparses a quote" $ do
      Unparser.block (AQuote [PlainText (T.pack "Here's a quote")])
        `shouldBe` T.pack "> Here's a quote"
    it "unparses a blank line" $ do
      Unparser.block ABlank `shouldBe` T.pack ""
    it "unparses a tag" $ do
      Unparser.block (ATag (T.pack "ATag"))
        `shouldBe` T.pack "! ATag"
    it "unparses a key-value pair" $ do
      Unparser.block (AKeyValue (T.pack "Key") (T.pack "Value"))
        `shouldBe` T.pack "! Key Value"
    it "unparses a triple" $ do
      Unparser.block (ATriple (T.pack "Subject") (T.pack "Predicate") (T.pack "Object"))
        `shouldBe` T.pack "& Subject Predicate Object"
  describe "document" $ do
    it "unparses a document" $ do
      Unparser.document
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
              (SlashLink . documentName . T.pack) "punctuated-equilibrium",
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
        `shouldBe` T.pack "# Overview\n\nEvolution is a behavior that emerges in any system with:\n\n- Mutation\n- Heredity\n- Selection\n\nEvolutionary systems often generate unexpected solutions. Nature selects for good enough.\n\n> There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you\8217re living in. (Olivia Judson, Santa Fe Institute https://overcast.fm/+UtNTAcN2Y/13:36 )\n\nEvolving systems exist in /punctuated-equilibrium.\n\n# Questions\n\n- What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.\n- What happens to an evolutionary system when you remove mutation? Heredity? Selection?\n- Do you see a system with one of these properties? How can you introduce the other two?\n\n\n# See also\n\nhttps://en.wikipedia.org/wiki/Evolutionary_systems"
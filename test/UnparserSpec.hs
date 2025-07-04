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
      Unparser.unparseInline (PlainText (T.pack "Hello, world!"))
        `shouldBe` T.pack "Hello, world!"
    it "unparses a bare URL" $ do
      Unparser.unparseInline (BareUrl (T.pack "https://google.com"))
        `shouldBe` T.pack "https://google.com"
    it "unparses an angle-delimited URL" $ do
      Unparser.unparseInline (AngledUrl (T.pack "doi:10.1000/100"))
        `shouldBe` T.pack "<doi:10.1000/100>"
    it "unparses a slashlink" $ do
      Unparser.unparseInline ((SlashLink . documentName . T.pack) "slash-link")
        `shouldBe` T.pack "/slash-link"
  describe "block" $ do
    it "unparses a paragraph" $ do
      Unparser.unparseBlock
        ( Paragraph
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
      Unparser.unparseBlock (Heading (T.pack "Heading"))
        `shouldBe` T.pack "# Heading"
    it "unparses a bullet" $ do
      Unparser.unparseBlock
        ( Bullet
            [ PlainText (T.pack "Here's another slashlink: "),
              (SlashLink . documentName . T.pack) "electric/boogaloo"
            ]
        )
        `shouldBe` T.pack "- Here's another slashlink: /electric/boogaloo"
    it "unparses a quote" $ do
      Unparser.unparseBlock (Quote [PlainText (T.pack "Here's a quote")])
        `shouldBe` T.pack "> Here's a quote"
    it "unparses a blank line" $ do
      Unparser.unparseBlock Blank `shouldBe` T.pack ""
    it "unparses a tag" $ do
      Unparser.unparseBlock (Tag (T.pack "Tag"))
        `shouldBe` T.pack "! Tag"
    it "unparses a key-value pair" $ do
      Unparser.unparseBlock (KeyValue (T.pack "Key") (T.pack "Value"))
        `shouldBe` T.pack "! Key Value"
    it "unparses a triple" $ do
      Unparser.unparseBlock (Triple (T.pack "Subject") (T.pack "Predicate") (T.pack "Object"))
        `shouldBe` T.pack "& Subject Predicate Object"
  describe "unparseBlockOrRefs" $ do
    it "unparses a unparseBlockOrRef]" $ do
      let blocks = [ 
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
      let blockOrRefs = Left <$> blocks
      Unparser.unparseBlockOrRefs blockOrRefs
        `shouldBe` T.pack "# Overview\n\nEvolution is a behavior that emerges in any system with:\n\n- Mutation\n- Heredity\n- Selection\n\nEvolutionary systems often generate unexpected solutions. Nature selects for good enough.\n\n> There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you\8217re living in. (Olivia Judson, Santa Fe Institute https://overcast.fm/+UtNTAcN2Y/13:36 )\n\nEvolving systems exist in /punctuated-equilibrium.\n\n# Questions\n\n- What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.\n- What happens to an evolutionary system when you remove mutation? Heredity? Selection?\n- Do you see a system with one of these properties? How can you introduce the other two?\n\n\n# See also\n\nhttps://en.wikipedia.org/wiki/Evolutionary_systems"
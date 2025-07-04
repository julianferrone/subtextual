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
  describe "parseNonBlankBlock" $ do
    it "parses a paragraph" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "Hello, world!")
        $ Left . Paragraph $ [PlainText (T.pack "Hello, world!")]
    it "parses a HTTP URL" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "http://www.google.com")
        $ Left . Paragraph $ [BareUrl (T.pack "http://www.google.com")]
    it "parses a HTTPS URL" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "https://www.google.com")
        $ Left . Paragraph $ [BareUrl (T.pack "https://www.google.com")]
    it "parses HTTP and angle-delimited URLs" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "This is a HTTP URL: https://www.google.com, and this is an angle URL: <doi:10.1000/100>")
        $ Left . Paragraph $
            [ PlainText (T.pack "This is a HTTP URL: "),
              BareUrl (T.pack "https://www.google.com"),
              PlainText (T.pack ", and this is an angle URL: "),
              AngledUrl (T.pack "doi:10.1000/100")
            ]
    it "parses a heading" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "# Header")
        $ Left . Heading $ T.pack "Header"
    it "parses a bullet" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "- Bullet")
        $ Left . Bullet $ [PlainText (T.pack "Bullet")]
    it "parses a quote" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "> Quote")
        $ Left . Quote $ [PlainText (T.pack "Quote")]
    it "parses a slashlink" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "/foo/bar")
        $ Left . Paragraph $ [(SlashLink . documentName . T.pack) "foo/bar"]
    it "parses a tag" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "! Tag")
        $ Left . Tag $ T.pack "Tag"
    it "parses a key value pair" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "! Key Value")
        $ Left $ KeyValue (T.pack "Key") (T.pack "Value")
    it "parses a key value pair with spaces in the value" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "! Key Value 2")
        $ Left $ KeyValue (T.pack "Key") (T.pack "Value 2")
    it "parses a subject object predicate" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "& Subject Predicate Object")
        $ Left $ Triple (T.pack "Subject") (T.pack "Predicate") (T.pack "Object")
    it "parses a transclusion with the WholeDocument option" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "$ notes")
        $ Right $ Transclusion (T.pack "notes") WholeDocument
    it "parses a transclusion with the FirstLines option" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "$ notes | 5")
        $ Right $ Transclusion (T.pack "notes") (FirstLines 5)
    it "parses a transclusion with the Lines option" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "$ notes | 5 10")
        $ Right $ Transclusion (T.pack "notes") (Lines 5 10)
    it "parses a transclusion with the WholeDocument option" $ do
      shouldMatch
        Parser.parseNonBlankBlock
        (T.pack "$ notes # heading 1")
        $ Right $ Transclusion (T.pack "notes") (HeadingSection . T.pack "heading 1")
  describe "document" $ do
    it "parses a whole document" $ do
      shouldMatch
        Parser.parseDocument
        (T.pack "# Overview\n\nEvolution is a behavior that emerges in any system with:\n\n- Mutation\n- Heredity\n- Selection\n\nEvolutionary systems often generate unexpected solutions. Nature selects for good enough.\n\n> There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you\8217re living in. (Olivia Judson, Santa Fe Institute https://overcast.fm/+UtNTAcN2Y/13:36 )\n\nEvolving systems exist in /punctuated-equilibrium.\n\n# Questions\n\n- What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.\n- What happens to an evolutionary system when you remove mutation? Heredity? Selection?\n- Do you see a system with one of these properties? How can you introduce the other two?\n\n\n# See also\n\nhttps://en.wikipedia.org/wiki/Evolutionary_systems")
        (Left <$> [ Heading (T.pack "Overview"),
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
              (SlashLink . documentName) (T.pack "punctuated-equilibrium"),
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
        ])
module TransclusionSpec (spec) where

import qualified Data.Text as Text
import qualified Subtextual.Core as Core
import qualified Subtextual.Transclusion as Transclusion
import Test.Hspec

testDoc :: [Core.Resolved]
testDoc =
  Core.Present
    <$> [ Core.Heading (Text.pack "Overview"),
          Core.Blank,
          Core.Paragraph [Core.PlainText (Text.pack "Evolution is a behavior that emerges in any system with:")],
          Core.Blank,
          Core.Bullet [Core.PlainText (Text.pack "Mutation")],
          Core.Bullet [Core.PlainText (Text.pack "Heredity")],
          Core.Bullet [Core.PlainText (Text.pack "Selection")],
          Core.Blank,
          Core.Paragraph [Core.PlainText (Text.pack "Evolutionary systems often generate unexpected solutions. Nature selects for good enough.")],
          Core.Blank,
          Core.Quote
            [ Core.PlainText (Text.pack "There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you’re living in. (Olivia Judson, Santa Fe Institute "),
              Core.BareUrl (Text.pack "https://overcast.fm/+UtNTAcN2Y/13:36"),
              Core.PlainText (Text.pack " )")
            ],
          Core.Blank,
          Core.Paragraph
            [ Core.PlainText (Text.pack "Evolving systems exist in "),
              (Core.SlashLink . Core.documentName) (Text.pack "punctuated-equilibrium"),
              Core.PlainText (Text.pack ".")
            ],
          Core.Blank,
          Core.Heading (Text.pack "Questions"),
          Core.Blank,
          Core.Bullet [Core.PlainText (Text.pack "What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.")],
          Core.Bullet [Core.PlainText (Text.pack "What happens to an evolutionary system when you remove mutation? Heredity? Selection?")],
          Core.Bullet [Core.PlainText (Text.pack "Do you see a system with one of these properties? How can you introduce the other two?")],
          Core.Blank,
          Core.Blank,
          Core.Heading (Text.pack "See also"),
          Core.Blank,
          Core.Paragraph [Core.BareUrl (Text.pack "https://en.wikipedia.org/wiki/Evolutionary_systems")]
        ]

spec :: Spec
spec = do
  describe "excerpt" $ do
    it "excerpting the whole document returns the whole document" $ do
      Transclusion.excerpt Core.WholeDocument testDoc
        `shouldBe` Just testDoc
    it "excerpting the first 7 lines returns lines 0-6" $ do
      Transclusion.excerpt (Core.FirstLines 7) testDoc
        `shouldBe` Just (Core.Present
          <$> [ Core.Heading (Text.pack "Overview"),
                Core.Blank,
                Core.Paragraph [Core.PlainText (Text.pack "Evolution is a behavior that emerges in any system with:")],
                Core.Blank,
                Core.Bullet [Core.PlainText (Text.pack "Mutation")],
                Core.Bullet [Core.PlainText (Text.pack "Heredity")],
                Core.Bullet [Core.PlainText (Text.pack "Selection")]
              ])
    it "excerpting lines 7 4 returns lines 7-10" $ do
      Transclusion.excerpt (Core.Lines 7 4) testDoc
        `shouldBe` Just (Core.Present
          <$> [ Core.Blank,
                Core.Paragraph [Core.PlainText (Text.pack "Evolutionary systems often generate unexpected solutions. Nature selects for good enough.")],
                Core.Blank,
                Core.Quote
                  [ Core.PlainText (Text.pack "There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you’re living in. (Olivia Judson, Santa Fe Institute "),
                    Core.BareUrl (Text.pack "https://overcast.fm/+UtNTAcN2Y/13:36"),
                    Core.PlainText (Text.pack " )")
                  ]
              ])
    it "excerpting existing heading returns the section under the heading" $ do
      Transclusion.excerpt (Core.HeadingSection (Text.pack "Questions")) testDoc
        `shouldBe` Just (Core.Present
          <$> [ Core.Heading (Text.pack "Questions"),
                Core.Blank,
                Core.Bullet [Core.PlainText (Text.pack "What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.")],
                Core.Bullet [Core.PlainText (Text.pack "What happens to an evolutionary system when you remove mutation? Heredity? Selection?")],
                Core.Bullet [Core.PlainText (Text.pack "Do you see a system with one of these properties? How can you introduce the other two?")],
                Core.Blank,
                Core.Blank
              ])
    it "excerpting non-existing heading returns Nothing" $ do
      Transclusion.excerpt (Core.HeadingSection (Text.pack "Heading doesn't exist")) testDoc
        `shouldBe` Nothing
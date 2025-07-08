module TransclusionSpec (spec) where

import Data.Either

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

acyclicTestCorpus :: Transclusion.Corpus Core.Authored
acyclicTestCorpus = Transclusion.fromDocuments [
  Core.document (Core.documentName (Text.pack "foo")) [
      Core.Raw (Core.Paragraph [Core.PlainText (Text.pack "This is line 0 of foo")]),
      Core.Raw (Core.Heading (Text.pack "Foo 1")),
      Core.Raw (Core.Paragraph [Core.PlainText (Text.pack "This is under the Foo 1 heading")]),
      Core.Raw (Core.Tag (Text.pack "underFoo1HeadingToo"))
    ],
  Core.document (Core.documentName (Text.pack "bar")) [
      Core.Raw (Core.Paragraph [Core.PlainText (Text.pack "Bar 0")]),
      Core.Raw (Core.Paragraph [Core.PlainText (Text.pack "Bar 1")]),
      Core.Raw (Core.Paragraph [Core.PlainText (Text.pack "Bar 2")]),
      Core.Raw (Core.Paragraph [Core.PlainText (Text.pack "Bar 3")]),
      Core.Raw (Core.Paragraph [Core.PlainText (Text.pack "Bar 4")])
    ],
  Core.document (Core.documentName (Text.pack "lorem-ipsum")) [
      Core.Raw (Core.Bullet [Core.PlainText (Text.pack "Lorem ipsum dolor sit amet")]),
      Core.Raw (Core.Bullet [Core.PlainText (Text.pack "Consectetur adipiscing elit")]),
      Core.Raw (Core.Bullet [Core.PlainText (Text.pack "Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua")])
    ],
  Core.document (Core.documentName (Text.pack "transclusions")) [
      Core.ToResolve (Core.Transclusion (Core.documentName (Text.pack "foo")) (Core.HeadingSection (Text.pack "Foo 1"))),
      Core.ToResolve (Core.Transclusion (Core.documentName (Text.pack "bar")) (Core.FirstLines 3)),
      Core.ToResolve (Core.Transclusion (Core.documentName (Text.pack "lorem-ipsum")) (Core.Lines 1 1))
    ]
  ]

transclusionsDoc :: Core.Document Core.Resolved
transclusionsDoc = Core.document (Core.documentName (Text.pack "transclusions")) [
    Core.Present (Core.Heading (Text.pack "Foo 1")),
    Core.Present (Core.Paragraph [Core.PlainText (Text.pack "This is under the Foo 1 heading")]),
    Core.Present (Core.Tag (Text.pack "underFoo1HeadingToo")),
    Core.Present (Core.Paragraph [Core.PlainText (Text.pack "Bar 0")]),
    Core.Present (Core.Paragraph [Core.PlainText (Text.pack "Bar 1")]),
    Core.Present (Core.Paragraph [Core.PlainText (Text.pack "Bar 2")]),
    Core.Present (Core.Bullet [Core.PlainText (Text.pack "Consectetur adipiscing elit")])
  ]

justRight :: Either a b -> Maybe b
justRight (Left _) = Nothing
justRight (Right r) = Just r

spec :: Spec
spec = do
  describe "excerpt" $ do
    it "excerpting the whole document returns the whole document" $ do
      Transclusion.excerpt Core.WholeDocument testDoc
        `shouldBe` Right testDoc
    it "excerpting the first 7 lines returns lines 0-6" $ do
      Transclusion.excerpt (Core.FirstLines 7) testDoc
        `shouldBe` Right (Core.Present
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
        `shouldBe` Right (Core.Present
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
        `shouldBe` Right (Core.Present
          <$> [ Core.Heading (Text.pack "Questions"),
                Core.Blank,
                Core.Bullet [Core.PlainText (Text.pack "What systems (beside biology) exhibit evolutionary behavior? Remember, evolution happens in any system with mutation, heredity, selection.")],
                Core.Bullet [Core.PlainText (Text.pack "What happens to an evolutionary system when you remove mutation? Heredity? Selection?")],
                Core.Bullet [Core.PlainText (Text.pack "Do you see a system with one of these properties? How can you introduce the other two?")],
                Core.Blank,
                Core.Blank
              ])
    it "excerpting non-existing heading returns Left" $ do
      Transclusion.excerpt (Core.HeadingSection (Text.pack "Heading doesn't exist")) testDoc
        `shouldBe` Left (Text.pack "Heading doesn't exist")
  describe "resolveCorpus" $ do
    it "resolves the acyclic corpus" $ 
      Transclusion.resolveCorpus acyclicTestCorpus `shouldSatisfy` isRight
    it "resolves the acyclic corpus and looks up the transcluded document" $
      case Transclusion.resolveCorpus acyclicTestCorpus of
        Right resolved -> 
          Transclusion.lookupDocument (Core.documentName (Text.pack "transclusions")) resolved
            `shouldBe` Just transclusionsDoc
        err -> err `shouldSatisfy` isRight 
        -- this shouldn't occur, we know the acyclic
        -- graph should be resolved
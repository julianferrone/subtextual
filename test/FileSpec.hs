module FileSpec (spec) where

import qualified Data.Text as T
import Subtextual.Core
import qualified Subtextual.File as File
import qualified System.FilePath as FP
import System.IO.Temp
import Test.Hspec

testDoc :: Document Authored
testDoc =
  document
    ((documentName . T.pack) "document")
    $ Raw
      <$> [ Heading (T.pack "Overview"),
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

spec :: Spec
spec = do
  around (withSystemTempDirectory "tmp") $ do
    describe "end to end" $ do
      it "successfully writes a Document to a file and reads the same Document from the file" $ \dir -> do
        _ <- File.writeAuthoredUnderDir dir testDoc
        read <- File.readSubtext (dir FP.</> "document.subtext")
        read `shouldBe` Right testDoc
module FileSpec (spec) where

import Subtextual.Core
import qualified Subtextual.File as File
import System.IO.Temp

import qualified Data.Text as T
import qualified System.FilePath as FP

import Test.Hspec


spec :: Spec
spec = do
    around (withSystemTempDirectory "tmp") $ do
        describe "end to end" $ do
            it "successfully writes a AuthoredDocument to a file and reads the same AuthoredDocument from the file" $ \dir -> do
                let doc = [
                        AHeading (T.pack "Overview"),
                        ABlank,
                        AParagraph [PlainText (T.pack "Evolution is a behavior that emerges in any system with:")],
                        ABlank,
                        ABullet [PlainText (T.pack "Mutation")],
                        ABullet [PlainText (T.pack "Heredity")],
                        ABullet [PlainText (T.pack "Selection")],
                        ABlank,
                        AParagraph [PlainText (T.pack "Evolutionary systems often generate unexpected solutions. Nature selects for good enough.")],
                        ABlank,
                        AQuote [
                            PlainText (T.pack "There is no such thing as advantageous in a general sense. There is only advantageous for the circumstances you’re living in. (Olivia Judson, Santa Fe Institute "),
                            BareUrl (T.pack "https://overcast.fm/+UtNTAcN2Y/13:36"),
                            PlainText (T.pack " )")
                        ],
                        ABlank,
                        AParagraph [
                            PlainText (T.pack "Evolving systems exist in "),
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
                let filepath = dir FP.</> "document.subtext"
                _ <- File.writeSubtext filepath doc
                read <- File.readSubtext filepath
                read `shouldBe` Right ("document", doc)
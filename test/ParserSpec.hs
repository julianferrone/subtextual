module ParserSpec (spec) where

import qualified Parser

import qualified Data.Text as T
import Data.Attoparsec.Text

import Subtext
import Test.Hspec

shouldMatch :: (Show a, Eq a) => Parser a -> T.Text -> a -> IO ()
shouldMatch parser input result = parseOnly parser input `shouldBe` Right result

spec :: Spec
spec = do
    describe "Parser" $ do
        it "parses a paragraph" $ do
            shouldMatch 
                Parser.block 
                (T.pack "Hello, world!")
                (Paragraph [PlainText (T.pack "Hello, world!")])
        it "parses a HTTP URL" $ do
            shouldMatch
                Parser.block
                (T.pack "http://www.google.com")
                (Paragraph [BareUrl (T.pack "http://www.google.com")])      
        it "parses a HTTPS URL" $ do
            shouldMatch
                Parser.block
                (T.pack "https://www.google.com")
                (Paragraph [BareUrl (T.pack "https://www.google.com")])         
        it "parses HTTP and angle-delimited URLs" $ do
            shouldMatch
                Parser.block
                (T.pack "This is a HTTP URL: https://www.google.com, and this is an angle URL: <doi:10.1000/100>")
                (Paragraph [
                    PlainText (T.pack "This is a HTTP URL: "),
                    BareUrl (T.pack "https://www.google.com"),
                    PlainText (T.pack ", and this is an angle URL: "),
                    AngledUrl (T.pack "doi:10.1000/100")
                ])
        it "parses a heading" $ do
            shouldMatch
                Parser.block
                (T.pack "# Header")
                (Heading (T.pack "Header"))

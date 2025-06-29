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
        

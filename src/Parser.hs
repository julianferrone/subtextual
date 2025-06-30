module Parser
    (block, document) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Text as T

import Subtext

------------------------------------------------------------
--                      Text Parsing                      --
------------------------------------------------------------

whitespace :: Parser T.Text
whitespace = takeWhile1 (\c -> c == ' ' || c == '\t')

word :: Parser T.Text
word = takeWhile1 $ not . isSpace

------------------------------------------------------------
--                     Inline Parsing                     --
------------------------------------------------------------

plainText :: Parser Inline
plainText = fmap PlainText $ word <|> whitespace

isUrlChar :: Char -> Bool
isUrlChar c = not $ c == '>' || isSpace c

string' :: String -> Parser T.Text
string' = string . T.pack

bareUrl :: Parser Inline
bareUrl = do
    schema <- string' "https://" <|> string' "http://"
    body <- manyTill anyChar $ lookAhead endOfUrl
    let url = schema <> T.pack body
    return $ BareUrl url

    where
        endOfUrl :: Parser ()
        endOfUrl = 
            punctuationBoundary 
            <|> space $> () 
            <|> endOfInput

        punctuationBoundary :: Parser ()
        punctuationBoundary = do
            c1 <- char '.' <|> char ';' <|> char ','
            c2 <- skip isSpace <|> endOfLine
            return ()



isAngledUrlChar :: Char -> Bool
isAngledUrlChar c = not $ c == '<' || c == '>' || isSpace c

angledUrl :: Parser Inline
angledUrl = do
    string' "<"
    url <- takeWhile1 isAngledUrlChar
    string' ">"
    return $ AngledUrl url

inline :: Parser Inline
inline = 
    bareUrl
    <|> angledUrl
    <|> plainText

inlines :: Parser [Inline]
inlines = do
    parsed <- many inline
    let parsed' = smoosh parsed []
    return parsed'
    where
        smoosh :: [Inline] -> [Inline] -> [Inline]
        smoosh [] finished = reverse finished
        smoosh (PlainText p : todo) (PlainText p' : done) = smoosh todo $ PlainText (p' <> p) : done
        smoosh (i : todo) done = smoosh todo (i : done)

------------------------------------------------------------
--                      Block Parsing                     --
------------------------------------------------------------

----------                 Helpers                ----------

prefixed :: Char -> Parser a -> Parser a
prefixed c parser = char c *> skipSpace *> parser

takeUntilEndOfLine :: Parser T.Text
takeUntilEndOfLine = takeWhile1 $ not . isEndOfLine

----------              Block Parsing             ----------

paragraph :: Parser Block
paragraph = Paragraph <$> inlines

heading :: Parser Block
heading = Heading <$> prefixed '#' takeUntilEndOfLine

bullet :: Parser Block
bullet = Bullet <$> prefixed '-' inlines

quote :: Parser Block
quote = Quote <$> prefixed '>' inlines

blank :: Parser Block
blank = do
    endOfLine
    endOfLine
    return Blank

block :: Parser Block
block = 
    heading
    <|> bullet
    <|> quote
    <|> paragraph
    <|> blank

------------------------------------------------------------
--                    Document Parsing                    --
------------------------------------------------------------

document :: Parser Document
document = many block
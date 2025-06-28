module Parser
    ( ) where

import Data.Char

import Data.Attoparsec.Text
import qualified Data.Text as T

import Subtext

------------------------------------------------------------
--                      Text Parsing                      --
------------------------------------------------------------

whitespace :: Parser T.Text
whitespace = takeWhile1 isSpace

word :: Parser T.Text
word = takeWhile1 $ \x -> not $ isSpace x

------------------------------------------------------------
--                     Inline Parsing                     --
------------------------------------------------------------

------------------------------------------------------------
--                      Block Parsing                     --
------------------------------------------------------------

------------------------------------------------------------
--                    Document Parsing                    --
------------------------------------------------------------
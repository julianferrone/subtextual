module Subtextual.File (readDocument) where

import Subtextual.Core
import qualified Subtextual.Parser as P

import qualified Data.Text.IO as I
import qualified System.FilePath as FP
import Data.Attoparsec.Text (parseOnly)

------------------------------------------------------------
--                 Selecting Subtext Files                --
------------------------------------------------------------

isSubtextFile :: FilePath -> Bool
isSubtextFile = (".subtext" ==) . FP.takeExtension

------------------------------------------------------------
--                  Reading Subtext Files                 --
------------------------------------------------------------

readDocument :: FilePath -> IO (Either String Document)
readDocument fp = do 
    file <- I.readFile fp
    let doc = parseOnly P.document file
    return doc


module Subtextual.File (readDocument) where

import Data.Attoparsec.Text (parseOnly)
import Subtextual.Core (Document)

import qualified Data.Text.IO as I
import qualified Subtextual.Parser as P
import qualified System.FilePath as FP
import qualified System.Directory as D

------------------------------------------------------------
--                 Selecting Subtext Files                --
------------------------------------------------------------

isSubtextFile :: FilePath -> Bool
isSubtextFile = (".subtext" ==) . FP.takeExtension

subtextFilesInDir :: FilePath -> IO [FilePath]
subtextFilesInDir dir = do
    children <- D.listDirectory dir
    let subtextFiles = filter isSubtextFile children
    return subtextFiles

------------------------------------------------------------
--                  Reading Subtext Files                 --
------------------------------------------------------------

readDocument :: FilePath -> IO (Either String Document)
readDocument fp = do 
    file <- I.readFile fp
    let doc = parseOnly P.document file
    return doc

readDocuments :: FilePath -> IO [Either String Document]
readDocuments dir = do
    subtextFiles <- subtextFilesInDir dir
    mapM readDocument subtextFiles
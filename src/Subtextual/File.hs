module Subtextual.File (
    readDocument, 
    readDocuments, 
    writeDocument, 
    writeDocuments
) where

import Data.Attoparsec.Text (parseOnly)
import Subtextual.Core (Document)

import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as I
import qualified Subtextual.Html as H
import qualified Subtextual.Parser as P
import qualified Subtextual.Unparser as U
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

------------------------------------------------------------
--                  Writing Subtext Files                 --
------------------------------------------------------------

write' :: (Document -> T.Text) -> FilePath -> Document -> IO ()
write' f fp doc = I.writeFile fp $ f doc

----------                 Subtext                ----------

writeDocument :: FilePath -> Document -> IO ()
writeDocument = write' U.document

writeDocuments :: FilePath -> [(String, Document)] -> IO ()
writeDocuments dir namedDocs = mapM_ (uncurry writeDocument) (qualifyPaths dir namedDocs)
    where
        qualifyPaths :: FilePath -> [(String, Document)] -> [(FilePath, Document)]
        qualifyPaths fp namedDocs = [(dir FP.</> name, doc) | (name, doc) <- namedDocs]

----------                  HTML                  ----------

writeDocumentHtml :: FilePath -> Document -> IO ()
writeDocumentHtml = write' H.renderDoc

writeDocumentsHtml :: FilePath -> [(String, Document)] -> IO ()
writeDocumentsHtml dir namedDocs = mapM_ (uncurry writeDocumentHtml) (qualifyPaths dir namedDocs)
    where
        qualifyPaths :: FilePath -> [(String, Document)] -> [(FilePath, Document)]
        qualifyPaths fp namedDocs = [(dir FP.</> name, doc) | (name, doc) <- namedDocs]
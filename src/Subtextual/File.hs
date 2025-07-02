module Subtextual.File (
    readSubtext, 
    readSubtexts, 
    writeSubtext, 
    writeSubtexts
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

readSubtext :: FilePath -> IO (Either String (String, Document))
readSubtext fp = do 
    file <- I.readFile fp
    let result = parseOnly P.document file
    case result of
        Left err -> return $ Left err
        Right doc' -> do
            let docName = FP.takeBaseName fp
            return $ Right (docName, doc')

readSubtexts :: FilePath -> IO [Either String (String, Document)]
readSubtexts dir = do
    subtextFiles <- subtextFilesInDir dir
    mapM readSubtext subtextFiles

------------------------------------------------------------
--                  Writing Subtext Files                 --
------------------------------------------------------------

write' :: 
    (Document -> T.Text)  -- Render the Document as Text
    -> FilePath           -- Filepath to write the Document to
    -> Document           -- The Document
    -> IO ()              -- Writing the file
write' f fp doc = I.writeFile fp $ f doc

qualifyPaths :: 
    FilePath                  -- The parent directory
    -> [(String, Document)]   -- The named documents
    -> [(FilePath, Document)] -- The filepaths for the documents
qualifyPaths parentDir namedDocs = [(parentDir FP.</> name, doc) | (name, doc) <- namedDocs]

writes' :: 
    (FilePath -> Document -> IO ()) -- Write a Document to a filepath
    -> FilePath                     -- Filepath of the parent directory
    -> [(String, Document)]         -- The list of named Documents
    -> IO ()                        -- Writing the file
writes' writeF parentDir namedDocs = 
    mapM_ 
    (uncurry writeF) 
    (qualifyPaths parentDir namedDocs)

----------                 Subtext                ----------

writeSubtext :: FilePath -> Document -> IO ()
writeSubtext = write' U.document

writeSubtexts :: FilePath -> [(String, Document)] -> IO ()
writeSubtexts = writes' writeSubtext

----------                  HTML                  ----------

writeHtml :: FilePath -> Document -> IO ()
writeHtml = write' H.renderDoc

writeHtmls :: FilePath -> [(String, Document)] -> IO ()
writeHtmls = writes' writeHtml
{-# LANGUAGE LambdaCase #-}

module Subtextual.File
  ( readSubtext,
    readSubtexts,
    writeSubtext,
    writeSubtexts,
    transcribeSubtextDirToHtml,
  )
where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.ByteString as ByteString
import Data.Either
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as IO
import qualified Subtextual.Core as Core
import qualified Subtextual.Html as Html
import qualified Subtextual.Parser as Parser
import qualified Subtextual.Transclusion as Transclusion
import qualified Subtextual.Unparser as Unparser
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

------------------------------------------------------------
--                         UTF8 IO                        --
------------------------------------------------------------

readFileUtf8 :: FilePath -> IO Text.Text
readFileUtf8 fp = Encoding.decodeUtf8 <$> ByteString.readFile fp

writeFileUtf8 :: FilePath -> Text.Text -> IO ()
writeFileUtf8 fp text = ByteString.writeFile fp (Encoding.encodeUtf8 text)

------------------------------------------------------------
--                 Selecting Subtext Files                --
------------------------------------------------------------

isSubtextFile :: FilePath -> Bool
isSubtextFile = (".subtext" ==) . FilePath.takeExtension

subtextFilesInDir :: FilePath -> IO [FilePath]
subtextFilesInDir dir = do
  children <- Directory.listDirectory dir
  let subtextFiles = filter isSubtextFile children
  return subtextFiles

------------------------------------------------------------
--                          Paths                         --
------------------------------------------------------------

documentPath ::
  FilePath -> -- Filepath to the root directory
  Core.Document a -> -- Document
  FilePath -- Filepath to the Document
documentPath rootDir doc = rootDir FilePath.</> ((Text.unpack . Core.unDocumentName . Core.title $ doc) <> ".subtext")

------------------------------------------------------------
--                  Reading Subtext Files                 --
------------------------------------------------------------

readSubtext ::
  FilePath -> -- Path to the Subtext file
  IO
    ( Either
        String -- Failure message during parsing
        (Core.Document Core.Authored) -- successfully parsed Document Authored
    )
readSubtext fp = do
  file <- readFileUtf8 fp
  let result = parseOnly Parser.parseAuthoreds file
  case result of
    Left errMsg -> return $ Left errMsg
    Right doc' -> do
      let docName = Core.documentName . Text.pack . FilePath.takeBaseName $ fp
      return . Right $ Core.document docName doc'

readSubtexts ::
  FilePath -> -- Path to the root directory
  IO
    ( [String], -- List of failure messages (during parsing)
      Transclusion.Corpus Core.Authored -- Corpus Authored made of successfully
      -- parsed documents
    )
readSubtexts dir = do
  subtextFiles <- subtextFilesInDir dir
  fs <- mapM readSubtext subtextFiles
  let (failureMsgs, documents) = partitionEithers fs
  let corpusAuthored = Transclusion.fromDocuments documents
  return (failureMsgs, corpusAuthored)

------------------------------------------------------------
--                         Writing                        --
------------------------------------------------------------

----------            Writing Document            ----------

-- Write the content of a Document to a given filepath
writeDocContentToPath ::
  (Core.Document a -> Text.Text) -> -- Render the document as Text
  FilePath -> -- The path to write the Document to
  Core.Document a -> -- The Document
  IO ()
writeDocContentToPath render fp doc = writeFileUtf8 fp $ render doc

-- Write a Document under a given root directory.
writeDocUnderDir ::
  (Core.Document a -> Text.Text) -> -- Render the document as Text
  FilePath -> -- The root directory to write under
  Core.Document a -> -- The Document to write
  IO ()
writeDocUnderDir render rootDir doc = writeDocContentToPath render (documentPath rootDir doc) doc

------------------------------------------------------------
--                 Piping Subtext to HTML                 --
------------------------------------------------------------

{-
TODO: Fix so we transclude documents before piping to HTML
I think we'll have to remove transcribing just one
document at a time, as it might contain a transclusion reference.

Instead we'll only allow operating on entire corpuses at once.
-}

transcribeSubtextDirToHtml :: FilePath -> FilePath -> IO [Either String ()]
transcribeSubtextDirToHtml = _

-- transcribeSubtextDirToHtml srcDir dstDir = do
--   subtexts <- readSubtexts srcDir
--   let qualifyRightPath = fmap $ qualifyPath dstDir
--   let namedDocs = qualifyRightPath <$> subtexts
--   mapM
--     ( \case
--         Left err -> return (Left err)
--         Right (dst, doc) -> do
--           _ <- writeHtml dst doc
--           return (Right ())
--     )
--     namedDocs
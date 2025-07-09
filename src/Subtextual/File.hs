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
--                  Writing Subtext Files                 --
------------------------------------------------------------

write' ::
  (a -> Text.Text) -> -- Render the Document as Text
  FilePath -> -- Filepath to write the Document to
  a -> -- The Document
  IO () -- Writing the file
write' f fp doc = writeFileUtf8 fp $ f doc

qualifyPath ::
  FilePath ->
  (String, a) ->
  (FilePath, a)
qualifyPath parentDir (name, doc) = (parentDir FilePath.</> name, doc)

qualifyPaths ::
  FilePath -> -- The parent directory
  [(String, a)] -> -- The named documents
  [(FilePath, a)] -- The filepaths for the documents
qualifyPaths parentDir = map (qualifyPath parentDir)

writes' ::
  (FilePath -> a -> IO ()) -> -- Write a Document to a filepath
  FilePath -> -- Filepath of the parent directory
  [(String, a)] -> -- The list of named Documents
  IO () -- Writing the file
writes' writeF parentDir namedDocs =
  mapM_
    (uncurry writeF)
    (qualifyPaths parentDir namedDocs)

----------                 Subtext                ----------

writeSubtext :: FilePath -> [Core.Authored] -> IO ()
writeSubtext = write' Unparser.unparseAuthoreds

writeSubtexts :: FilePath -> [(String, [Core.Authored])] -> IO ()
writeSubtexts = writes' writeSubtext

----------                  HTML                  ----------

writeHtml :: FilePath -> Core.Document Core.Resolved -> IO ()
writeHtml = write' Html.renderDoc

writeHtmls :: FilePath -> [(String, Core.Document Core.Resolved)] -> IO ()
writeHtmls = writes' writeHtml

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
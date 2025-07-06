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
import qualified Data.ByteString as B (readFile, writeFile)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as I
import Subtextual.Core (Authored, Document)
import qualified Subtextual.Html as H
import qualified Subtextual.Parser as P
import qualified Subtextual.Unparser as U
import qualified System.Directory as D
import qualified System.FilePath as FP

------------------------------------------------------------
--                         UTF8 IO                        --
------------------------------------------------------------

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 fp = E.decodeUtf8 <$> B.readFile fp

writeFileUtf8 :: FilePath -> T.Text -> IO ()
writeFileUtf8 fp text = B.writeFile fp (E.encodeUtf8 text)

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

readSubtext :: FilePath -> IO (Either String (String, [Authored]))
readSubtext fp = do
  file <- readFileUtf8 fp
  let result = parseOnly P.parseAuthoreds file
  case result of
    Left err -> return $ Left err
    Right doc' -> do
      let docName = FP.takeBaseName fp
      return $ Right (docName, doc')

readSubtexts :: FilePath -> IO [Either String (String, [Authored])]
readSubtexts dir = do
  subtextFiles <- subtextFilesInDir dir
  mapM readSubtext subtextFiles

------------------------------------------------------------
--                  Writing Subtext Files                 --
------------------------------------------------------------

write' ::
  (a -> T.Text) -> -- Render the Document as Text
  FilePath -> -- Filepath to write the Document to
  a -> -- The Document
  IO () -- Writing the file
write' f fp doc = writeFileUtf8 fp $ f doc

qualifyPath ::
  FilePath ->
  (String, a) ->
  (FilePath, a)
qualifyPath parentDir (name, doc) = (parentDir FP.</> name, doc)

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

writeSubtext :: FilePath -> [Authored] -> IO ()
writeSubtext = write' U.unparseAuthoreds

writeSubtexts :: FilePath -> [(String, [Authored])] -> IO ()
writeSubtexts = writes' writeSubtext

----------                  HTML                  ----------

writeHtml :: FilePath -> Document -> IO ()
writeHtml = write' H.renderDoc

writeHtmls :: FilePath -> [(String, Document)] -> IO ()
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
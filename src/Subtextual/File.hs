{-# LANGUAGE LambdaCase #-}

module Subtextual.File
  ( readSubtext,
    readSubtexts,
    writeSubtext,
    writeSubtexts,
    transcribeSubtextToHtml,
    transcribeSubtextDirToHtml,
  )
where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.ByteString as B (readFile, writeFile)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as I
import Subtextual.Core (AuthoredDocument)
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

readSubtext :: FilePath -> IO (Either String (String, AuthoredDocument))
readSubtext fp = do
  file <- readFileUtf8 fp
  let result = parseOnly P.document file
  case result of
    Left err -> return $ Left err
    Right doc' -> do
      let docName = FP.takeBaseName fp
      return $ Right (docName, doc')

readSubtexts :: FilePath -> IO [Either String (String, AuthoredDocument)]
readSubtexts dir = do
  subtextFiles <- subtextFilesInDir dir
  mapM readSubtext subtextFiles

------------------------------------------------------------
--                  Writing Subtext Files                 --
------------------------------------------------------------

write' ::
  (AuthoredDocument -> T.Text) -> -- Render the AuthoredDocument as Text
  FilePath -> -- Filepath to write the AuthoredDocument to
  AuthoredDocument -> -- The AuthoredDocument
  IO () -- Writing the file
write' f fp doc = writeFileUtf8 fp $ f doc

qualifyPath ::
  FilePath ->
  (String, AuthoredDocument) ->
  (FilePath, AuthoredDocument)
qualifyPath parentDir (name, doc) = (parentDir FP.</> name, doc)

qualifyPaths ::
  FilePath -> -- The parent directory
  [(String, AuthoredDocument)] -> -- The named documents
  [(FilePath, AuthoredDocument)] -- The filepaths for the documents
qualifyPaths parentDir = map (qualifyPath parentDir)

writes' ::
  (FilePath -> AuthoredDocument -> IO ()) -> -- Write a AuthoredDocument to a filepath
  FilePath -> -- Filepath of the parent directory
  [(String, AuthoredDocument)] -> -- The list of named Documents
  IO () -- Writing the file
writes' writeF parentDir namedDocs =
  mapM_
    (uncurry writeF)
    (qualifyPaths parentDir namedDocs)

----------                 Subtext                ----------

writeSubtext :: FilePath -> AuthoredDocument -> IO ()
writeSubtext = write' U.document

writeSubtexts :: FilePath -> [(String, AuthoredDocument)] -> IO ()
writeSubtexts = writes' writeSubtext

----------                  HTML                  ----------

writeHtml :: FilePath -> AuthoredDocument -> IO ()
writeHtml = write' H.renderDoc

writeHtmls :: FilePath -> [(String, AuthoredDocument)] -> IO ()
writeHtmls = writes' writeHtml

------------------------------------------------------------
--                 Piping Subtext to HTML                 --
------------------------------------------------------------

transcribeSubtextToHtml :: FilePath -> FilePath -> IO (Either String ())
transcribeSubtextToHtml src dst = do
  subtext <- readSubtext src
  case subtext of
    Left err -> return (Left err)
    Right (_, doc) -> do
      _ <- writeHtml dst doc
      return (Right ())

transcribeSubtextDirToHtml :: FilePath -> FilePath -> IO [Either String ()]
transcribeSubtextDirToHtml srcDir dstDir = do
  subtexts <- readSubtexts srcDir
  let qualifyRightPath = fmap $ qualifyPath dstDir
  let namedDocs = qualifyRightPath <$> subtexts
  mapM
    ( \case
        Left err -> return (Left err)
        Right (dst, doc) -> do
          _ <- writeHtml dst doc
          return (Right ())
    )
    namedDocs
{-# LANGUAGE LambdaCase #-}

module Subtextual.File
  ( readSubtext,
    readSubtexts,
    writeAuthoredToPath,
    writeAuthoredUnderDir,
    writeAuthoredCorpus,
    writeHtmlToPath,
    writeHtmlUnderDir,
    writeHtmlCorpus,
    transcribeSubtextFileToHtml,
    transcribeSubtextDirToHtml,
  )
where

import Control.Monad
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
    Left errMsg -> return . Left $ "Failed to parse file: " <> fp <> " due to: " <> errMsg
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

----------            Writing Generics            ----------

------ Write Document a

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

------ Write Corpus a

writeCorpusToDir ::
  (Core.Document a -> Text.Text) -> -- Render a Document as Text
  FilePath -> -- The root directory of the corpus
  Transclusion.Corpus a -> -- The list of Documents to write
  IO ()
writeCorpusToDir render rootDir corpus' =
  mapM_
    (writeDocUnderDir render rootDir)
    (Transclusion.toDocuments corpus')

----------      Unparsing and Writing Subtext     ----------

writeAuthoredToPath :: FilePath -> Core.Document Core.Authored -> IO ()
writeAuthoredToPath = writeDocContentToPath (Unparser.unparseAuthoreds . Core.content)

writeAuthoredUnderDir :: FilePath -> Core.Document Core.Authored -> IO ()
writeAuthoredUnderDir = writeDocUnderDir (Unparser.unparseAuthoreds . Core.content)

writeAuthoredCorpus :: FilePath -> Transclusion.Corpus Core.Authored -> IO ()
writeAuthoredCorpus = writeCorpusToDir (Unparser.unparseAuthoreds . Core.content)

----------  Rendering Subtext to HTML and Writing ----------

writeHtmlToPath :: FilePath -> Core.Document Core.Resolved -> IO ()
writeHtmlToPath = writeDocContentToPath Html.renderDoc

writeHtmlUnderDir :: FilePath -> Core.Document Core.Resolved -> IO ()
writeHtmlUnderDir = writeDocUnderDir Html.renderDoc

writeHtmlCorpus :: FilePath -> Transclusion.Corpus Core.Resolved -> IO ()
writeHtmlCorpus = writeCorpusToDir Html.renderDoc

------------------------------------------------------------
--                 Piping Subtext to HTML                 --
------------------------------------------------------------

transcribeSubtextFileToHtml ::
  FilePath -> -- The source Subtext filepath to read from
  FilePath -> -- The target HTML filepath to write to
  -- Left error if the file wasn't parsed,
  -- Right () if the file was parsed and written
  IO (Either String ())
transcribeSubtextFileToHtml src dst = do
  subtext <- readSubtext src
  case subtext of
    Left errorMsg -> return $ Left errorMsg
    Right docAuthored -> do
      let docResolved = Core.resolveWithoutLookup docAuthored
      writeHtmlToPath dst docResolved
      return $ Right ()

transcribeSubtextDirToHtml ::
  FilePath -> -- The source directory of Subtext files to read from
  FilePath -> -- The target directory of HTML files to write to
  IO
    ( Either
        (Transclusion.GraphContainsCycles Core.DocumentName) -- The graph is cyclic
        [String] -- The list of error messages during parsing
    )
transcribeSubtextDirToHtml srcDir dstDir = do
  (parserErrorMsgs, corpus) <- readSubtexts srcDir
  case Transclusion.resolveCorpus corpus of
    Left cycles -> return $ Left cycles
    Right resolvedCorpus -> do
      writeHtmlCorpus dstDir resolvedCorpus
      return $ Right parserErrorMsgs

-- return parserErrorMsgs
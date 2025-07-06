module Main (main) where

import qualified Options.Applicative as A
import qualified Subtextual.File as F
import qualified System.FilePath as FP

------------------------------------------------------------
--                      CLI Arguments                     --
------------------------------------------------------------

----------                Arguments               ----------

newtype Options = Options {optCommand :: Command} deriving (Show)

data Command = ConvertDirectoryToHtml FilePath FilePath
  deriving (Show)

----------                 Parsing                ----------

convertDirectoryToHtml :: A.Parser Command
convertDirectoryToHtml =
  ConvertDirectoryToHtml
    <$> A.argument
      A.str
      ( A.metavar "SUBTEXT_DIR_FILEPATH"
          <> A.help "The filepath of the directory of Subtext files to read."
      )
    <*> A.argument
      A.str
      ( A.metavar "HTML_DIR_FILEPATH"
          <> A.help "The filepath of the directory to write the rendered HTML files to."
      )

------ Options Parsing

options :: A.Parser Options
options =
  Options
    <$> A.hsubparser
      ( A.command
          "dir"
          ( A.info
              convertDirectoryToHtml
              ( A.progDesc "Render a directory of Subtext files into HTML."
              )
          )
      )

----------                Executing               ----------

------ Executing Command

printConversionMessage :: String -> String -> Either String () -> IO ()
printConversionMessage src dst (Left err) = do
  putStrLn $
    "Converting Subtext "
      <> src
      <> " to HTML "
      <> dst
      <> " failed: "
      <> err
  return ()
printConversionMessage src dst (Right res) = do
  putStrLn $
    "Converting Subtext "
      <> src
      <> " to HTML "
      <> dst
      <> " succeeded."
  return ()

runCommand :: Command -> IO ()
-- runCommand (ConvertFileToHtml src dst) = do
--     result <- F.transcribeSubtextToHtml src dst
--     printConversionMessage src dst result

runCommand (ConvertDirectoryToHtml srcDir dstDir) = do
  result <- F.transcribeSubtextDirToHtml srcDir dstDir
  mapM_ (printConversionMessage srcDir dstDir) result

------ Executing Options

runOptions :: Options -> IO ()
runOptions = runCommand . optCommand

------------------------------------------------------------
--                        Main CLI                        --
------------------------------------------------------------

main :: IO ()
main = do
  opts <-
    A.execParser $
      A.info
        (options A.<**> A.helper)
        ( A.fullDesc
            <> A.progDesc "A command-line tool for working with Subtext, the text-based line-oriented hypertext format designed for note-taking."
            <> A.header "Subtextual CLI"
        )
  runOptions opts

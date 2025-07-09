module Main (main) where

import qualified Options.Applicative as A
import qualified Subtextual.File as F
import qualified System.FilePath as FP

------------------------------------------------------------
--                      CLI Arguments                     --
------------------------------------------------------------

----------                Arguments               ----------

newtype Options = Options {optCommand :: Command} deriving (Show)

data Command
  = ConvertFileToHtml FilePath FilePath
  | ConvertDirectoryToHtml FilePath FilePath
  deriving (Show)

----------                 Parsing                ----------

convertFileToHtml :: A.Parser Command
convertFileToHtml =
  ConvertFileToHtml
    <$> A.argument
      A.str
      ( A.metavar "SUBTEXT_FILEPATH"
          <> A.help "The filepath of the Subtext file to read."
      )
    <*> A.argument
      A.str
      ( A.metavar "HTML_FILEPATH"
          <> A.help "The filepath to write the rendered HTML file to."
      )

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
          "file"
          ( A.info
              convertFileToHtml
              (A.progDesc "Render a Subtext file into HTML.")
          )
          <> A.command
            "dir"
            ( A.info
                convertDirectoryToHtml
                (A.progDesc "Render a directory of Subtext files into HTML.")
            )
      )

----------                Executing               ----------

------ Executing Command

runCommand :: Command -> IO ()
-- runCommand (ConvertFileToHtml src dst) = do
--     result <- F.transcribeSubtextToHtml src dst
--     printConversionMessage src dst result

runCommand (ConvertFileToHtml src dst) = do
  result <- F.transcribeSubtextFileToHtml src dst
  case result of
    Left errorMsg -> putStrLn $ "Failed to parse " <> src <> ": " <>  errorMsg
    Right _ -> putStrLn $ "Successfully transcribed " <> src <> " to " <> dst

runCommand (ConvertDirectoryToHtml srcDir dstDir) = do
  result <- F.transcribeSubtextDirToHtml srcDir dstDir
  case result of
    Left cycles -> putStrLn $ "Failed to resolve graph due to cycles: " <> show cycles
    Right errorMsgs -> mapM_ (putStrLn . ("Failed to parse: " <>)) errorMsgs

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

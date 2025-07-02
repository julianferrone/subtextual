module Main (main) where

import qualified Subtextual.File as F
import qualified Options.Applicative as A
import qualified System.FilePath as FP

------------------------------------------------------------
--                      CLI Arguments                     --
------------------------------------------------------------

----------                Arguments               ----------

newtype Options = Options {optCommand :: Command} deriving Show

data Command = 
    ConvertFileToHtml FilePath FilePath
    | ConvertDirectoryToHtml FilePath FilePath
    deriving Show

----------                 Parsing                ----------

------ Command Parsing

convertFileToHtml :: A.Parser Command
convertFileToHtml = 
    ConvertFileToHtml 
    <$> A.argument A.str (
        A.metavar "SUBTEXT_FILEPATH" 
        <> A.help "The filepath of the Subtext file to read."
        )
    <*> A.argument A.str (
        A.metavar "HTML_FILEPATH" 
        <> A.help "The filepath to write out the rendered HTML file to."
        )

convertDirectoryToHtml :: A.Parser Command
convertDirectoryToHtml = 
    ConvertDirectoryToHtml
    <$> A.argument A.str (
        A.metavar "SUBTEXT_DIR_FILEPATH" 
        <> A.help "The filepath of the directory of Subtext files to read."
        )
    <*> A.argument A.str (
        A.metavar "HTML_DIR_FILEPATH" 
        <> A.help "The filepath of the directory to write the rendered HTML files to."
        )

------ Options Parsing

options :: A.Parser Options
options = Options <$> A.hsubparser
    ( A.command "file" (A.info convertFileToHtml (A.progDesc "Render a Subtext file into HTML.") )
    <> A.command "dir" (A.info convertDirectoryToHtml (A.progDesc "Render a directory of Subtext files into HTML.") )
    )

----------                Executing               ----------

printConversionMessage :: String -> String -> Either String () -> IO ()
printConversionMessage src dst (Left err) = do
    putStrLn $ "Converting Subtext '" 
        <> src 
        <> "' to HTML '" 
        <> dst
        <> "' failed: "
        <> err
    return ()
printConversionMessage src dst (Right res) = do
    putStrLn $ "Converting Subtext " 
        <> src 
        <> "to HTML succeeded."
    return ()

runCommand :: Command -> IO ()
runCommand (ConvertFileToHtml src dst) = do
    result <- F.transcribeSubtextToHtml src dst
    printConversionMessage src dst result
        
runCommand (ConvertDirectoryToHtml srcDir dstDir) = do
    result <- F.transcribeSubtextDirToHtml srcDir dstDir
    mapM_ (printConversionMessage srcDir dstDir) result

------------------------------------------------------------
--                        Main CLI                        --
------------------------------------------------------------

main :: IO ()
main = do
    opts <- A.execParser 
        $ A.info 
        (options A.<**> A.helper) 
        (A.fullDesc
        <> A.progDesc "Test program description"
        <> A.header "Test header"
        )
    print opts

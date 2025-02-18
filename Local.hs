module Main (main) where

import qualified Data.Aeson as A
import qualified GPGDecryption
import qualified Data.ByteString.Lazy as BL
import System.Exit (die)
import Options.Applicative (Parser, strArgument, metavar, help, info, helper, progDesc, fullDesc, header, execParser)
import Control.Applicative ((<**>))

newtype FilePathArg = FilePathArg { filePath :: FilePath }
  deriving (Show)

filePathParser :: Parser FilePathArg
filePathParser = FilePathArg
  <$> strArgument
      ( metavar "FILE"
     <> help "Path to the input file" )

main :: IO ()
main = do

  let
    opts = info (filePathParser <**> helper)
      ( fullDesc
     <> progDesc "Reads a file path argument and prints it"
     <> header "Simple File Path Argument Parser" )

  filePathArg <- execParser opts
  
  jsonData <- BL.readFile $ filePath filePathArg

  -- Parse JSON
  case A.decode jsonData :: Maybe A.Value of
    Just jsonPayload -> do
      result <- GPGDecryption.handleDecrypt jsonPayload
      print result
    Nothing -> die "failed to parse JSON"


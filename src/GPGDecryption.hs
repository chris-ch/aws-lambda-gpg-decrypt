{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module GPGDecryption (
    handleDecrypt, DecryptionRequest(..), decryptGPG, processRecord
) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified System.Log.FastLogger as LOG

import qualified AWSEvent (RecordSet(..), Record(..))
import qualified Helper
import System.IO.Temp         (withSystemTempDirectory)
import qualified Data.ByteString as BS

import System.FilePath ((</>))
import GPGme.Ctx (withCtx, setPassphraseCallback)
import GPGme.Types ( Protocol(OpenPGP), Ctx, Encrypted )
import GPGme.Key (importKeyFromFile)
import GPGme.Crypto (decrypt)
import Control.Monad (forM)
import qualified Data.ByteString.Lazy as BSL
import System.Exit (die)
import qualified Data.Text.Encoding as T
import Data.Aeson (Options(fieldLabelModifier))

import System.Directory (makeAbsolute)
import Helper (parseS3Uri)
data DecryptionResponse = DecryptionComplete T.Text | DecryptionError T.Text
    deriving (Show, Generic)
instance A.ToJSON DecryptionResponse

data DecryptionRequest = DecryptionRequest
    { s3InputPath  :: T.Text
    , s3GpgKeyPath :: T.Text
    , passphrase   :: T.Text
    , s3OutputPath :: T.Text
    } deriving (Show, Generic)

camelToSnake :: String -> String
camelToSnake = A.camelTo2 '_'

instance A.FromJSON DecryptionRequest where
    parseJSON = A.genericParseJSON A.defaultOptions { fieldLabelModifier = camelToSnake }

instance A.ToJSON DecryptionRequest where
    toJSON = A.genericToJSON A.defaultOptions { fieldLabelModifier = camelToSnake }

withPassphraseCb :: Ctx -> String -> IO ()
withPassphraseCb ctx passphrase = do
  setPassphraseCallback ctx (Just callback)
  where
    callback _ _ _ = return (Just passphrase)

decryptGPG :: T.Text -> BS.ByteString -> Encrypted -> IO BS.ByteString
decryptGPG passphrase keyFileData encryptedData= do
      loggerSet <- LOG.newStderrLoggerSet LOG.defaultBufSize

      withSystemTempDirectory "gpg-temp-homedir" $ \tempDir -> do
        let
            privateKeyPath = tempDir </> "private.key"

        BS.writeFile privateKeyPath keyFileData
        logMessage loggerSet $ "private key locally saved under: " <> T.pack privateKeyPath
        withCtx tempDir "C" OpenPGP $ \ctx -> do
            withPassphraseCb ctx (T.unpack passphrase)

            -- Import the private key
            mbImportResult <- importKeyFromFile ctx privateKeyPath

            case mbImportResult of
                Nothing -> return ()
                Just impResult -> do
                    keyFileAbsPath <- makeAbsolute privateKeyPath
                    die $ "failed to import key from \"" <> keyFileAbsPath <> "\" (" <> show impResult <> ")"

            logMessage loggerSet "private key imported"

            -- Decrypt the data
            withPassphraseCb ctx (T.unpack passphrase)
            result <- decrypt ctx encryptedData
            case result of
                Left err -> die $ "decryption failed: " ++ show err
                Right decrypted -> return decrypted

processRecord :: DecryptionRequest -> IO DecryptionResponse
processRecord request =  do
        loggerSet <- LOG.newStderrLoggerSet LOG.defaultBufSize
        let
            mbInputPath = parseS3Uri $ s3InputPath request
            mbKeyPath = parseS3Uri $ s3GpgKeyPath request
            mbOutputPath = parseS3Uri $ s3OutputPath request
        case (mbInputPath, mbKeyPath, mbOutputPath) of
            (Just (inputBucket, inputPath),
                Just (keyBucket, keyPath),
                Just (outputBucket, outputPath)
                ) -> do
                let
                    ppKey = passphrase request
                logMessage loggerSet $ T.pack "loading encrypted object: " <> T.pack (show inputPath) <> " from bucket " <> T.pack (show inputBucket)
                encrypted <- Helper.loadContentFromS3 inputBucket inputPath
                logMessage loggerSet $ T.pack "loading private key: " <> T.pack (show keyPath) <> " from bucket " <> T.pack (show keyBucket)
                pKey <- Helper.loadContentFromS3 keyBucket keyPath
                decrypted <- decryptGPG ppKey pKey encrypted
                logMessage loggerSet $ T.pack "saving decrypted object: " <> T.pack (show outputPath)
                Helper.writeToS3 outputBucket outputPath decrypted "application/octet-stream"
                return $ DecryptionComplete $ "decryption saved to " <> outputBucket <> " as object " <> outputPath
            _ -> die $ "unrecognized request: " <> show request

handleDecrypt :: A.Value -> IO [DecryptionResponse]
handleDecrypt sqsEvent = do
    loggerSet <- LOG.newStderrLoggerSet LOG.defaultBufSize
    logMessage loggerSet $ "processing SQS event: " <> T.pack (show sqsEvent)

    -- First parse the outer structure
    case A.fromJSON sqsEvent of
        A.Success (AWSEvent.RecordSet records) -> do
            -- For each record, parse the body field which contains JSON string
            forM records $ \record -> do
                let
                    encodedRequest = (BSL.fromStrict . T.encodeUtf8 . AWSEvent.body) record
                    mbRequest = A.decode encodedRequest
                case mbRequest of
                    Nothing -> die $ "failed to parse json: " <> show encodedRequest
                    Just req ->  processRecord req

        A.Error err -> do
            logMessage loggerSet $ "failed to parse input event: " <> T.pack err
            return [DecryptionError $ "failed to parse input event: " <> T.pack err]

logMessage :: LOG.LoggerSet -> T.Text -> IO ()
logMessage loggerSet msg = do
    LOG.pushLogStrLn loggerSet . LOG.toLogStr $ msg


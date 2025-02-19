module GPGDecryptSpec (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import System.Environment (getEnv)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe)
import GPGDecryption
    ( decryptGPG, processRecord, DecryptionRequest(..) )
import Helper ( loadContentFromS3 )

spec :: Spec
spec = do
  describe "GPG Decryption" $ do
    it "loads a private key from S3" $ do
        let
            keyFile = "resources" </> "test" </> "private-ibrokers-reporting.gpg"

        refKeyFileData <- BS.readFile keyFile
        content <- loadContentFromS3 "unit-tests-underline1-cascade6" "private-ibrokers-reporting.pgp"
        content `shouldBe` refKeyFileData

    it "loads an encrypted file from S3" $ do
        let
            encryptedFile = "resources" </> "test" </> "sample-encrypted.gpg"

        refEncryptedData <- BS.readFile encryptedFile
        content <- loadContentFromS3 "unit-tests-underline1-cascade6" "ibrokers-positions-encrypted/F929289.Risk_Reporting_Export.20250205.20250205.xml.pgp"
        content `shouldBe` refEncryptedData

    it "decrypts a file" $ do

      let
        keyFile = "resources" </> "test" </> "private-ibrokers-reporting.gpg"
        encryptedFile = "resources" </> "test" </> "sample-encrypted.gpg"

      passphrase <- getEnv "IB_PGP_PASS_KEY"
      keyFileData <- BS.readFile keyFile
      encryptedData <- BS.readFile encryptedFile

      decrypted <- decryptGPG (T.pack passphrase) keyFileData encryptedData
      T.take 63 (TE.decodeUtf8 decrypted) `shouldBe` "<FlexQueryResponse queryName=\"Risk Reporting Export\" type=\"AF\">"

    it "processes a record" $ do

      pp <- getEnv "IB_PGP_PASS_KEY"
      let request = DecryptionRequest
              { s3InputPath  = "s3://unit-tests-underline1-cascade6/ibrokers-positions-encrypted/F929289.Risk_Reporting_Export.20250205.20250205.xml.pgp"
              , s3GpgKeyPath = "s3://unit-tests-underline1-cascade6/private-ibrokers-reporting.pgp"
              , passphrase   = T.pack pp
              , s3OutputPath = "s3://unit-tests-underline1-cascade6/ibrokers-positions-decrypted/F929289.Risk_Reporting_Export.20250205.20250205.xml"
        }

      result <- processRecord request
      show result `shouldBe` "DecryptionComplete \"decryption saved to unit-tests-underline1-cascade6 as object ibrokers-positions-decrypted/F929289.Risk_Reporting_Export.20250205.20250205.xml\""


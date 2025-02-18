module Main (main) where

import Test.Hspec ( hspec, describe )

import qualified GPGDecryptSpec(spec)

main :: IO ()
main = hspec $ do
  describe "GPG Decryption" GPGDecryptSpec.spec

module Main (main) where

import AWS.Lambda.Runtime (mRuntime)

import qualified GPGDecryption

main :: IO ()
main = mRuntime GPGDecryption.handleDecrypt


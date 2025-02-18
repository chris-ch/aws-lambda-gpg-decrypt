module GPGme.Key (importKeyFromFile
    ) where

import Bindings.Gpgme
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8

import Foreign


import GPGme.Types
    ( 
      GpgmeError(..),
      Ctx(Ctx, _ctx) )

import GPGme.Internal
    ( noError,
      newDataBuffer )

-- | Import a key from a file, this happens in two steps: populate a
-- @gpgme_data_t@ with the contents of the file, import the @gpgme_data_t@
importKeyFromFile :: Ctx -- ^ context to operate in
                  -> FilePath -- ^ file path to read from
                  -> IO (Maybe GpgmeError)
importKeyFromFile Ctx {_ctx=ctxPtr} fp = do
    dataPtr <- newDataBuffer
    retOpen <- 
      BS.useAsCString (BSC8.pack fp) $ \cFp ->
        c'gpgme_data_new_from_file dataPtr cFp 1

    -- If we couldn’t open the file at all, bail out:
    if retOpen /= noError
      then do
        free dataPtr
        pure (Just (GpgmeError retOpen))
      else do
        ctx <- peek ctxPtr
        dat <- peek dataPtr
        retImport <- c'gpgme_op_import ctx dat
        free dataPtr

        -- If GPGME import blew up at top level, return that error:
        if retImport /= noError
          then pure (Just (GpgmeError retImport))
          else do
            -- Now check how many secrets were actually imported:
            importResPtr <- c'gpgme_op_import_result ctx
            if importResPtr == nullPtr
              then do
                pure (Just (GpgmeError 1))  -- fallback
              else do
                importRes <- peek importResPtr
                let numSec = c'_gpgme_op_import_result'secret_imported importRes
                if numSec > 0
                  then pure Nothing            -- success!
                  else do
                    -- No secret key was imported. Return an error of your choice:
                    -- e.g. passphrase mismatch, incomplete key, or “No secret key imported”.
                    pure (Just (GpgmeError 11)) -- 11 = GPG_ERR_BAD_PASSPHRASE, or pick some code


-- | A user ID consisting of a name, comment, and email address.
data UserId = UserId { userId         :: String
                     , userName       :: String
                     , userEmail      :: String
                     , userComment    :: String
                     }
            deriving (Ord, Eq, Show)

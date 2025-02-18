module GPGme.Internal ( checkError, collectResult, noError, newDataBuffer,
      fromProtocol ) where

import Bindings.Gpgme
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (createAndTrim)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS (defaultChunkSize)
import Foreign (castPtr, Ptr, malloc)
import Foreign.C.String (peekCString)
import System.IO.Unsafe (unsafePerformIO)

import GPGme.Types
    ( Protocol(..) )

-- | Read the buffer into a ByteString.
--
-- Chunks of Data.ByteString.Lazy.Internal.defaultChunkSize are allocated and
-- copied, until the gpgme_data_readbuffer read returns less than this.
-- Then the list of chunks are copied into a strict ByteString by way of a lazy
-- ByteString.
collectResult :: C'gpgme_data_t -> BS.ByteString
collectResult dat' = unsafePerformIO $ do
    -- make sure we start at the beginning
    _ <- c'gpgme_data_seek dat' 0 seekSet
    chunks <- go dat'
    pure $ LBS.toStrict (LBS.fromChunks chunks)
  where makeChunk :: C'gpgme_data_t -> IO BS.ByteString
        makeChunk dat = BS.createAndTrim chunkSize $ \buf -> do
          -- createAndTrim gives a Ptr Word8 but the gpgme functions wants a Ptr ()
          read_bytes <- c'gpgme_data_read dat (castPtr buf) (fromIntegral chunkSize)
          pure $ fromIntegral read_bytes

        go :: C'gpgme_data_t -> IO [BS.ByteString]
        go dat = do
          bs <- makeChunk dat
          if BS.length bs < chunkSize
          then pure [bs]
          else do
            bss <- go dat
            pure (bs : bss)

        seekSet = 0
        chunkSize = LBS.defaultChunkSize

checkError :: String -> C'gpgme_error_t -> IO ()
checkError fun gpgme_err =
    unless (gpgme_err == noError) $
           do errstr <- c'gpgme_strerror gpgme_err
              str <- peekCString errstr
              srcstr <- c'gpgme_strsource gpgme_err
              src <- peekCString srcstr
              error ("Fun: " ++ fun ++
                     ", Error: " ++ str ++
                     ", Source: " ++ show src)

noError :: Num a => a
noError = 0


fromProtocol :: (Num a) => Protocol -> a
fromProtocol CMS     =  c'GPGME_PROTOCOL_CMS
fromProtocol GPGCONF =  c'GPGME_PROTOCOL_GPGCONF
fromProtocol OpenPGP =  c'GPGME_PROTOCOL_OpenPGP
fromProtocol UNKNOWN =  c'GPGME_PROTOCOL_UNKNOWN

newDataBuffer :: IO (Ptr C'gpgme_data_t)
newDataBuffer = do
    resultBufPtr <- malloc
    checkError "data_new" =<< c'gpgme_data_new resultBufPtr
    return resultBufPtr

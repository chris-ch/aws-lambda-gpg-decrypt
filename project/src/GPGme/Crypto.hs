module GPGme.Crypto (decrypt) where

import Bindings.Gpgme
import qualified Data.ByteString as BS
import Foreign


import GPGme.Internal
    ( checkError, collectResult, newDataBuffer, noError )

import GPGme.Types
    ( toDecryptError, Ctx(Ctx, _ctx), DecryptError, Encrypted, Plain )

-- | Decrypts a ciphertext
decrypt :: Ctx -> Encrypted -> IO (Either DecryptError Plain)
decrypt = decryptIntern c'gpgme_op_decrypt

decryptIntern :: (C'gpgme_ctx_t
                    -> C'gpgme_data_t
                    -> C'gpgme_data_t
                    -> IO C'gpgme_error_t
                  )
                  -> Ctx
                  -> Encrypted
                  -> IO (Either DecryptError Plain)
decryptIntern dec_op Ctx {_ctx=ctxPtr} cipher = do
    -- init buffer with cipher
    cipherBufPtr <- malloc
    BS.useAsCString cipher $ \bs -> do
        let copyData = 1 -- gpgme shall copy data, as bytestring will free it
        let cipherlen = fromIntegral (BS.length cipher)
        ret <- c'gpgme_data_new_from_mem cipherBufPtr bs cipherlen copyData
        checkError "data_new_from_mem" ret
    cipherBuf <- peek cipherBufPtr

    -- init buffer for result
    resultBufPtr <- newDataBuffer
    resultBuf <- peek resultBufPtr

    ctx <- peek ctxPtr

    -- decrypt
    errcode <- dec_op ctx cipherBuf resultBuf

    let res = if errcode /= noError
                then Left  (toDecryptError errcode)
                else Right (collectResult resultBuf)

    free cipherBufPtr
    free resultBufPtr

    return res

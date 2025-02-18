module GPGme.Ctx ( withCtx, setPassphraseCallback ) where

import Bindings.Gpgme
import Control.Monad (when)
import Control.Exception (SomeException(SomeException), catch, throwIO, toException)
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.String
import Foreign.C.Types

import GPGme.Types
    ( HgpgmeException(HgpgmeException),
      Ctx(Ctx, _ctx),
      Protocol )
import GPGme.Internal
    ( checkError, fromProtocol )

-- | Creates a new 'GPGme.Types.GPGme.Types.Ctx' from a @homedirectory@, a @locale@
--   and a @protocol@. Needs to be freed with 'freeCtx', which
--   is why you are encouraged to use 'withCtx'.
newCtx :: String   -- ^ path to gpg homedirectory
       -> String   -- ^ locale
       -> GPGme.Types.Protocol -- ^ protocol
       -> IO GPGme.Types.Ctx
newCtx homedir localeStr protocol =
    do homedirPtr <- newCString homedir

       -- check version: necessary for initialization!!
       version <- c'gpgme_check_version nullPtr >>= peekCString

       -- create context
       ctxPtr <- malloc
       checkError "gpgme_new" =<< c'gpgme_new ctxPtr

       ctx <- peek ctxPtr

       -- find engine version
       engInfo <- c'gpgme_ctx_get_engine_info ctx >>= peek
       engVersion <- peekCString $ c'_gpgme_engine_info'version engInfo

       -- set locale
       locale <- newCString localeStr
       checkError "set_locale" =<< c'gpgme_set_locale ctx lcCtype locale

       -- set protocol in ctx
       checkError "set_protocol" =<< c'gpgme_set_protocol ctx
                                        (fromProtocol protocol)

       -- set homedir in ctx
       checkError "set_engine_info" =<< c'gpgme_ctx_set_engine_info ctx
                            (fromProtocol protocol) nullPtr homedirPtr

       return (GPGme.Types.Ctx ctxPtr version protocol engVersion)
    where lcCtype :: CInt
          lcCtype = 0

-- | Free a previously created 'GPGme.Types.GPGme.Types.Ctx'
freeCtx :: GPGme.Types.Ctx -> IO ()
freeCtx GPGme.Types.Ctx {_ctx=ctxPtr} =
    do ctx <- peek ctxPtr
       c'gpgme_release ctx
       free ctxPtr

-- | Runs the action with a new 'GPGme.Types.GPGme.Types.Ctx' and frees it afterwards
--
--   See 'newCtx' for a descrption of the parameters.
withCtx :: String        -- ^ path to gpg homedirectory
        -> String        -- ^ locale
        -> GPGme.Types.Protocol      -- ^ protocol
        -> (GPGme.Types.Ctx -> IO a) -- ^ action to be run with ctx
        -> IO a
withCtx homedir localeStr prot f = do
    ctx <- newCtx homedir localeStr prot
    catch
      ( do
        res <- f ctx
        freeCtx ctx
        return res
      )
      -- If an exception occurs, first free the GPG context
      -- and then throw our own exception to signal that
      -- the exception was caught and accounted for.
      ( \(SomeException e) -> do
        freeCtx ctx
        throwIO $ GPGme.Types.HgpgmeException (toException e)
      )

-- | A callback invoked when the engine requires a passphrase to
-- proceed. The callback should return @Just@ the requested passphrase,
-- or @Nothing@ to cancel the operation.
type PassphraseCb =
       String     -- ^ user ID hint
    -> String     -- ^ passphrase info
    -> Bool       -- ^ @True@ if the previous attempt was bad
    -> IO (Maybe String)

-- | Construct a passphrase callback, handling reporting of the
-- passphrase back to gpgme.
passphraseCb :: PassphraseCb -> IO C'gpgme_passphrase_cb_t
passphraseCb callback = do
    let go _ hint info prev_bad fd = do
            hint' <- peekCString hint
            info' <- peekCString info
            result <- callback hint' info' (prev_bad /= 0)
            let phrase = fromMaybe "" result
            err <- withCStringLen (phrase++"\n") $ \(s,len) ->
                c'gpgme_io_writen fd (castPtr s) (fromIntegral len)
            when (err /= 0) $ checkError "passphraseCb" (fromIntegral err)
            return $ maybe errCanceled (const 0) result
        errCanceled = 99 -- TODO: Use constant
    mk'gpgme_passphrase_cb_t go

-- | Set the callback invoked when a passphrase is required from the user.
--
-- Note that the operation of this feature is a bit inconsistent between
-- GPG versions. GPG 1.4 using the @use-agent@ option and GPG >= 2.1 require
-- that the @gpg-agent@ for the session has the @allow-loopback-pinentry@
-- option enabled (this can be achieved by adding @allow-loopback-pinentry@
-- to @gpg-agent.conf@. GPG versions between 2.0 and 2.1 do not support the
-- @--pinentry-mode@ option necessary for this support.
--
-- See <http://lists.gnupg.org/pipermail/gnupg-devel/2013-February/027345.html>
-- and the @gpgme-tool@ example included in the @gpgme@ tree for details.
setPassphraseCallback :: GPGme.Types.Ctx                   -- ^ context
                      -> Maybe PassphraseCb    -- ^ a callback, or Nothing to disable
                      -> IO ()
setPassphraseCallback GPGme.Types.Ctx {_ctx=ctxPtr} callback = do
    ctx <- peek ctxPtr
    let mode = case callback of
                   Nothing -> c'GPGME_PINENTRY_MODE_DEFAULT
                   Just _  -> c'GPGME_PINENTRY_MODE_LOOPBACK
    -- With GPG 1.4 using the use-agent option and >= GPG 2.0 the passphrase
    -- callback won't have an opportunity to execute unless the loopback
    -- pinentry-mode is used
    c'gpgme_set_pinentry_mode ctx mode >>= checkError "setPassphraseCallback"
    cb <- maybe (return nullFunPtr) passphraseCb callback
    c'gpgme_set_passphrase_cb ctx cb nullPtr

module GPGme.Types ( toDecryptError, Ctx(Ctx, _ctx), DecryptError, Encrypted, Plain,
      Protocol(..),  GpgmeError(..),
      HgpgmeException(HgpgmeException),
        ) where

import Bindings.Gpgme
import qualified Data.ByteString as BS
import Foreign
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeException, Exception)

-- | the protocol to be used in the crypto engine
data Protocol =
      CMS
    | GPGCONF
    | OpenPGP
    | UNKNOWN
    deriving (Show, Eq, Ord)

-- | Context to be passed around with operations. Use 'newCtx' or
--   'withCtx' in order to obtain an instance.
data Ctx = Ctx {
      _ctx             :: Ptr C'gpgme_ctx_t -- ^ context
    , _version         :: String            -- ^ GPGME version
    , _protocol        :: Protocol          -- ^ context protocol
    , _engineVersion   :: String            -- ^ engine version
}

-- | Modes for signing with GPG
data SignMode = Normal | Detach | Clear deriving Show

-- | a plaintext
type Plain = BS.ByteString

-- | an ciphertext
type Encrypted = BS.ByteString

-- | the summary of a signature status
data SignatureSummary =
      BadPolicy                        -- ^ A policy requirement was not met
    | CrlMissing                       -- ^ The CRL is not available
    | CrlTooOld                        -- ^ Available CRL is too old
    | Green                            -- ^ The signature is good but one might want to display some extra information
    | KeyExpired                       -- ^ The key or one of the certificates has expired
    | KeyMissing                       -- ^ Can’t verify due to a missing key or certificate
    | KeyRevoked                       -- ^ The key or at least one certificate has been revoked
    | Red                              -- ^ The signature is bad
    | SigExpired                       -- ^ The signature has expired
    | SysError                         -- ^ A system error occured
    | UnknownSummary C'gpgme_sigsum_t  -- ^ The summary is something else
    | Valid                            -- ^ The signature is fully valid
    deriving (Show, Eq, Ord)

-- | Whether to include secret keys when searching
data IncludeSecret =
      WithSecret -- ^ do not include secret keys
    | NoSecret   -- ^ include secret keys
    deriving (Show, Eq, Ord)

data Flag =
      AlwaysTrust
    | NoFlag
    deriving (Show, Eq, Ord)

-- | A GPGME error.
--
-- Errors in GPGME consist of two parts: a code indicating the nature of the fault,
-- and a source indicating from which subsystem the error originated.
newtype GpgmeError = GpgmeError C'gpgme_error_t
                   deriving (Show, Ord, Eq)

-- | error indicating what went wrong in decryption
data DecryptError =
      NoData              -- ^ no data to decrypt
    | Failed              -- ^ not a valid cipher
    | BadPass             -- ^ passphrase for secret was wrong
    | Unknown GpgmeError  -- ^ something else went wrong
    deriving (Show, Eq, Ord)

toDecryptError :: C'gpgme_error_t -> DecryptError
toDecryptError n =
    case unsafePerformIO $ c'gpgme_err_code n of
        58   -> NoData
        152  -> Failed
        11   -> BadPass
        x    -> Unknown (GpgmeError x)

-- | The validity of a user identity
data Validity =
      ValidityUnknown
    | ValidityUndefined
    | ValidityNever
    | ValidityMarginal
    | ValidityFull
    | ValidityUltimate
    deriving (Show, Ord, Eq)

-- | A public-key encryption algorithm
data PubKeyAlgo =
      Rsa
    | RsaE
    | RsaS
    | ElgE
    | Dsa
    | Elg
    deriving (Show, Ord, Eq)

-- | h-gpgme exception for wrapping exception which occur outside of the control of h-gpgme
newtype HgpgmeException = HgpgmeException SomeException deriving (Show)
instance Exception HgpgmeException

-- | Flags for removeKey function
data RemoveKeyFlags = RemoveKeyFlags {
      allowSecret :: Bool -- ^ if False, only public keys are removed, otherwise secret keys are removed as well
    , force       :: Bool -- ^ if True, don't ask for confirmation
    } deriving (Show, Eq, Ord)

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Certificates obtained from an ACME server (e.g. Let's Encrypt) with
TLS-ALPN-01 challenges (RFC-8737), for the HTTPS server of @serve@ and
@multisite@.

The ACME server validates a domain by opening a TLS connection to port 443
with the @acme-tls/1@ protocol: the HTTPS server answers it from a
'TlsAlpn01Store' (see 'withAcmeChallenges'), so no port 80 is needed. A
certificate manager (from acme-not-a-joke-warp) obtains one certificate per
domain when the daemon starts, stores keys and certificates on disk, renews
them before they expire, and hands them to the running server, which presents
them at the next handshake without a restart.
-}
module KitchenSink.Engine.Acme (
    AcmeOptions (..),
    Acme,
    acmeDomains,
    setupAcme,
    setupAcmeOrExit,
    runAcme,
    acmeTlsSettings,
    withAcmeChallenges,
    acmeSniCredentials,
    servesDomain,
    resolveDirectory,
    defaultStateDir,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Network.TLS qualified as TLS
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Prelude ((||))

import Acme.NotAJoke.LetsEncrypt (letsencryptv2, staging_letsencryptv2)
import Acme.NotAJoke.Wai.TlsAlpn01 (TlsAlpn01Store, newTlsAlpn01Store, withTlsAlpn01)
import Acme.NotAJoke.Wai.WarpTLS (CertificateStore, liveTlsSettings, newCertificateStore, sniCredentials)
import Acme.NotAJoke.Warp.CertificateManager (
    CertificateManager,
    acmeTlsAlpn01Issuer,
    defaultAcmeSettings,
    defaultManagerSettings,
    newCertificateManager,
    runCertificateManager,
 )

import KitchenSink.Prelude

-- | How to reach the ACME server, shared by every domain of a daemon.
data AcmeOptions = AcmeOptions
    { directory :: Maybe Text
    {- ^ @letsencrypt@, @letsencrypt-staging@, or the URL of the directory of
    another ACME server. Required: there is no default, so that nobody gets
    staging certificates (untrusted) or hits production rate limits by
    accident.
    -}
    , contacts :: [Text]
    -- ^ contacts of the ACME account, @mailto:@ is added to bare addresses
    , stateDir :: Maybe FilePath
    -- ^ where the account key, the keys and the certificates live, see 'defaultStateDir'
    }
    deriving (Show)

-- | The running parts: challenge store, certificate store and their manager.
data Acme = Acme
    { domains :: NonEmpty Text
    , challenges :: TlsAlpn01Store
    , certificates :: CertificateStore
    , manager :: CertificateManager
    }

-- | The domains certificates are obtained for, lower-cased.
acmeDomains :: Acme -> NonEmpty Text
acmeDomains acme = acme.domains

-- | Where state is kept when @--acmeStateDir@ is not given (relative to the working directory).
defaultStateDir :: FilePath
defaultStateDir = "acme-state"

{- | Prepares a manager for some domains: one certificate per domain, the
first one also being presented to clients asking for no name or an unknown
one. Nothing is requested before 'runAcme'.
-}
setupAcme :: AcmeOptions -> NonEmpty Text -> IO (Either String Acme)
setupAcme opts names =
    case resolveDirectory =<< maybe (Left missingDirectory) Right opts.directory of
        Left err -> pure (Left err)
        Right url -> do
            let dir = fromMaybe defaultStateDir opts.stateDir
            let lowered = NonEmpty.nub (fmap Text.toLower names)
            store <- newTlsAlpn01Store
            certs <- newCertificateStore
            let acmeSettings = defaultAcmeSettings url (dir </> "account.jwk") (fmap asContact opts.contacts)
            created <- newCertificateManager (defaultManagerSettings lowered dir) (acmeTlsAlpn01Issuer acmeSettings store) certs
            pure $ fmap (Acme lowered store certs) created
  where
    missingDirectory =
        "ACME needs a directory: --acmeDirectory letsencrypt, letsencrypt-staging, or the URL of an ACME directory"

    asContact :: Text -> Text
    asContact c
        | ":" `Text.isInfixOf` c = c
        | otherwise = "mailto:" <> c

-- | 'setupAcme', printing the error and exiting on failure.
setupAcmeOrExit :: AcmeOptions -> NonEmpty Text -> IO Acme
setupAcmeOrExit opts names = do
    res <- setupAcme opts names
    case res of
        Left err -> do
            hPutStrLn stderr ("acme: " <> err)
            exitFailure
        Right acme -> do
            hPutStrLn stderr ("acme: managing certificates for " <> Text.unpack (Text.intercalate ", " (NonEmpty.toList acme.domains)) <> " (tls-alpn-01)")
            pure acme

{- | The base URL acme-not-a-joke wants (ending with @/@, @directory@ gets
appended to it) from a well-known name or a directory URL.
-}
resolveDirectory :: Text -> Either String Text
resolveDirectory txt =
    case Text.strip txt of
        "letsencrypt" -> Right letsencryptv2
        "letsencrypt-staging" -> Right staging_letsencryptv2
        url
            | "https://" `Text.isPrefixOf` url || "http://" `Text.isPrefixOf` url ->
                Right $ case Text.stripSuffix "directory" url of
                    Just base | "/" `Text.isSuffixOf` base -> base
                    _ | "/" `Text.isSuffixOf` url -> url
                    _ -> url <> "/"
            | otherwise ->
                Left ("unknown ACME directory " <> show url <> ": use letsencrypt, letsencrypt-staging, or an http(s) URL")

{- | Obtains, stores and renews the certificates, forever. Failures are
logged on stderr and retried later; they never stop the daemon.
-}
runAcme :: Acme -> IO ()
runAcme acme = runCertificateManager acme.manager

{- | warp-tls settings presenting the managed certificates and answering the
TLS-ALPN-01 challenges: what @serve@ uses.
-}
acmeTlsSettings :: Acme -> WarpTLS.TLSSettings
acmeTlsSettings acme = withAcmeChallenges acme (liveTlsSettings acme.certificates)

{- | Makes settings answer the TLS-ALPN-01 challenges. Apply it last: it
takes over the protocol choice and wraps the server name hook already set.
-}
withAcmeChallenges :: Acme -> WarpTLS.TLSSettings -> WarpTLS.TLSSettings
withAcmeChallenges acme = withTlsAlpn01 acme.challenges

{- | The managed credential for a server name: the one of that name, else the
one of the first domain (possibly none yet, until it is obtained).
-}
acmeSniCredentials :: Acme -> Maybe TLS.HostName -> IO TLS.Credentials
acmeSniCredentials acme = sniCredentials acme.certificates

-- | Whether a server name is one of the managed domains (case-insensitive).
servesDomain :: Acme -> TLS.HostName -> Bool
servesDomain acme name = Text.toLower (Text.pack name) `elem` acme.domains

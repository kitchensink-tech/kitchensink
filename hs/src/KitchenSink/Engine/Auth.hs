{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Authentication and access control for request-time dynamic pages (see
"KitchenSink.Engine.Dynamic"). This is the first cut described in the
feature's design notes: a single credential source (@config@: static users
in @kitchen-sink.json@, see 'KitchenSink.Engine.Config.AuthConfig'), no
forms\/POST (out of scope -- see 'KitchenSink.Engine.Dynamic'), and no roles
beyond public\/required.

Login is HTTP Basic against the @config@ provider (no login form needed);
on success a signed, expiring session cookie is set so the browser needn't
resend @Authorization@ on every request. Both the cookie and the
@Authorization@ header are checked on every request to a @\"required\"@
route; whichever is valid wins, cookie first.

Passwords are never stored in plaintext: 'hashPassword' produces a salted
PBKDF2-HMAC-SHA256 hash (@kitchen-sink hash-password@ is the CLI front-end
for it), and 'verifyPassword' checks a candidate password against that hash
in constant time.
-}
module KitchenSink.Engine.Auth (
    -- * access policy
    AuthPolicy (..),
    parseAuthPolicy,

    -- * identity
    Identity (..),

    -- * password hashing (config provider)
    hashPassword,
    verifyPassword,

    -- * session cookies
    sessionCookieName,
    signSession,
    verifySession,
    setSessionCookieHeader,

    -- * request-side auth
    authenticateConfigProvider,
    parseBasicAuthHeader,
    identityFromRequest,
    unauthorizedResponse,
) where

import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.KDF.PBKDF2 qualified as PBKDF2
import Crypto.MAC.HMAC qualified as HMAC
import Crypto.Random.Entropy (getEntropy)
import Data.ByteArray qualified as ByteArray
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Base64.URL qualified as Base64URL
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types (Header, status401)
import Network.Wai (Request)
import Network.Wai qualified as Wai
import Prelude (Integer, reads, round, (+), (>=))

import KitchenSink.Engine.Config (AuthConfig (..), AuthProviderConfig (..), AuthUser (..))
import KitchenSink.Prelude

-- | A dynamic page's access policy, from @=base:build-info.json@'s @auth@
-- field. Absent or unrecognized values fall back to 'Public', same
-- forward-compatible convention as an unknown @layout@ elsewhere in this
-- codebase.
data AuthPolicy = Public | Required
    deriving (Show, Eq)

parseAuthPolicy :: Maybe Text -> AuthPolicy
parseAuthPolicy (Just "required") = Required
parseAuthPolicy _ = Public

-- | The authenticated caller of a dynamic-page request. Only a username
-- today (the @config@ provider has no other attributes); exposed to
-- templates as @$ctx.request.user@ and bound as @:user_id@ in @.sql@
-- datasets (see "KitchenSink.Engine.Dynamic").
newtype Identity = Identity {identityUsername :: Text}
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- password hashing

pbkdf2Iterations :: Int
pbkdf2Iterations = 210000

pbkdf2KeyLength :: Int
pbkdf2KeyLength = 32

saltLength :: Int
saltLength = 16

-- | Salts and hashes a plaintext password for storage as an 'AuthUser's
-- @passwordHash@ (@kitchen-sink hash-password@ is the CLI front-end).
-- Never store the argument itself.
hashPassword :: Text -> IO Text
hashPassword password = do
    salt <- getEntropy saltLength :: IO ByteString
    pure (encodeHash pbkdf2Iterations salt (derive pbkdf2Iterations salt password))

-- | Checks a candidate password against a hash produced by 'hashPassword'.
-- A malformed hash (e.g. hand-edited config) never matches, rather than
-- erroring.
verifyPassword :: Text -> Text -> Bool
verifyPassword password storedHash = case decodeHash storedHash of
    Nothing -> False
    Just (iters, salt, expected) ->
        ByteArray.constEq expected (derive iters salt password)

derive :: Int -> ByteString -> Text -> ByteString
derive iters salt password =
    PBKDF2.generate
        (PBKDF2.prfHMAC SHA256)
        (PBKDF2.Parameters iters pbkdf2KeyLength)
        (Text.encodeUtf8 password)
        salt

encodeHash :: Int -> ByteString -> ByteString -> Text
encodeHash iters salt digest =
    Text.intercalate
        "$"
        [ "pbkdf2-sha256"
        , Text.pack (show iters)
        , Text.decodeUtf8 (Base64.encode salt)
        , Text.decodeUtf8 (Base64.encode digest)
        ]

decodeHash :: Text -> Maybe (Int, ByteString, ByteString)
decodeHash t = case Text.splitOn "$" t of
    ["pbkdf2-sha256", itersTxt, saltTxt, digestTxt] -> do
        iters <- readMaybeInt itersTxt
        salt <- hush (Base64.decode (Text.encodeUtf8 saltTxt))
        digest <- hush (Base64.decode (Text.encodeUtf8 digestTxt))
        pure (iters, salt, digest)
    _ -> Nothing

readMaybeInt :: Text -> Maybe Int
readMaybeInt t = case [n | (n, "") <- reads (Text.unpack t)] of
    [n] -> Just n
    _ -> Nothing

--------------------------------------------------------------------------------
-- session cookies

sessionCookieName :: ByteString
sessionCookieName = "ks_session"

-- | How long a session cookie is valid for, in seconds, once minted.
sessionLifetimeSeconds :: Integer
sessionLifetimeSeconds = 86400 -- 24h

-- | Signs a session identifying @username@, expiring
-- 'sessionLifetimeSeconds' from now. The cookie value is
-- @base64url(payload).base64url(hmac-sha256(secret, payload))@, where
-- @payload@ is @username|expiryEpochSeconds@.
signSession :: Text -> Text -> IO Text
signSession secret username = do
    now <- round <$> getPOSIXTime :: IO Integer
    let expiry = now + sessionLifetimeSeconds
    let payload = Text.encodeUtf8 (username <> "|" <> Text.pack (show expiry))
    pure (encodeSession secret payload)

encodeSession :: Text -> ByteString -> Text
encodeSession secret payload =
    let sig = ByteArray.convert (HMAC.hmac (Text.encodeUtf8 secret) payload :: HMAC.HMAC SHA256) :: ByteString
     in Text.decodeUtf8 (Base64URL.encodeUnpadded payload) <> "." <> Text.decodeUtf8 (Base64URL.encodeUnpadded sig)

-- | Verifies a session cookie value against 'secret', returning the
-- 'Identity' iff the signature checks out (constant time) and it hasn't
-- expired.
verifySession :: Text -> Text -> IO (Maybe Identity)
verifySession secret cookieValue = case Text.splitOn "." cookieValue of
    [payloadTxt, sigTxt] -> case (hush (Base64URL.decodeUnpadded (Text.encodeUtf8 payloadTxt)), hush (Base64URL.decodeUnpadded (Text.encodeUtf8 sigTxt))) of
        (Just payload, Just sig) -> do
            let expectedSig = ByteArray.convert (HMAC.hmac (Text.encodeUtf8 secret) payload :: HMAC.HMAC SHA256) :: ByteString
            if ByteArray.constEq sig expectedSig
                then do
                    now <- round <$> getPOSIXTime :: IO Integer
                    pure (checkExpiry now payload)
                else pure Nothing
        _ -> pure Nothing
    _ -> pure Nothing

checkExpiry :: Integer -> ByteString -> Maybe Identity
checkExpiry now payload = case Text.splitOn "|" (Text.decodeUtf8 payload) of
    [username, expiryTxt] -> do
        expiry <- readMaybeInteger expiryTxt
        if expiry >= now then Just (Identity username) else Nothing
    _ -> Nothing
  where
    readMaybeInteger :: Text -> Maybe Integer
    readMaybeInteger t = case [n | (n, "") <- reads (Text.unpack t)] of
        [n] -> Just n
        _ -> Nothing

-- | @Set-Cookie@ header for a freshly-minted session. @HttpOnly@ and
-- @SameSite=Strict@ always; @Secure@ iff the request that triggered the
-- login was itself HTTPS (never on a plain-HTTP dev server, since a
-- browser silently drops a @Secure@ cookie set over plain HTTP anyway, and
-- forcing it in dev would make login look broken).
setSessionCookieHeader :: Bool -> Text -> Header
setSessionCookieHeader secure signedValue =
    ( "Set-Cookie"
    , Text.encodeUtf8 $
        Text.decodeUtf8 sessionCookieName
            <> "="
            <> signedValue
            <> "; Path=/; HttpOnly; SameSite=Strict; Max-Age="
            <> Text.pack (show sessionLifetimeSeconds)
            <> (if secure then "; Secure" else "")
    )

--------------------------------------------------------------------------------
-- config provider

-- | Checks a username\/password pair against the @config@ provider's user
-- list.
authenticateConfigProvider :: AuthProviderConfig -> Text -> Text -> Maybe Identity
authenticateConfigProvider (ConfigProvider us) user pass =
    case List.find (\u -> u.username == user) us of
        Just u | verifyPassword pass u.passwordHash -> Just (Identity user)
        _ -> Nothing

-- | Parses an @Authorization: Basic <base64(user:pass)>@ header value.
parseBasicAuthHeader :: ByteString -> Maybe (Text, Text)
parseBasicAuthHeader h = do
    rest <- BS.stripPrefix "Basic " h
    decoded <- hush (Base64.decode rest)
    let (userBs, rest') = BS.break (== 58 {- ':' -}) decoded
    passBs <- BS.stripPrefix ":" rest'
    pure (Text.decodeUtf8 userBs, Text.decodeUtf8 passBs)

-- | Tries the session cookie first, then the @Authorization@ header,
-- against the given 'AuthConfig'. The second result is 'Just' a fresh
-- signed session cookie iff identity came from a freshly-presented Basic
-- credential (so the caller can set it on the response) -- 'Nothing' when
-- identity came from an already-valid cookie (nothing to refresh) or there
-- is no identity at all.
identityFromRequest :: AuthConfig -> Request -> IO (Maybe Identity, Maybe Text)
identityFromRequest cfg req = case cookieValue of
    Just v -> do
        mIdent <- verifySession cfg.cookieSecret v
        case mIdent of
            Just ident -> pure (Just ident, Nothing)
            Nothing -> viaBasic
    Nothing -> viaBasic
  where
    viaBasic = case List.lookup "Authorization" (Wai.requestHeaders req) >>= parseBasicAuthHeader of
        Just (user, pass) -> case authenticateConfigProvider cfg.provider user pass of
            Just ident@(Identity username) -> do
                signed <- signSession cfg.cookieSecret username
                pure (Just ident, Just signed)
            Nothing -> pure (Nothing, Nothing)
        Nothing -> pure (Nothing, Nothing)

    cookieValue :: Maybe Text
    cookieValue = do
        raw <- List.lookup "Cookie" (Wai.requestHeaders req)
        List.lookup sessionCookieName (parseCookieHeader raw)

parseCookieHeader :: ByteString -> [(ByteString, Text)]
parseCookieHeader raw =
    [ (k, Text.decodeUtf8 (BS.drop 1 v))
    | kv <- BS.split 59 {- ';' -} raw
    , let trimmed = BS.dropWhile (== 32 {- ' ' -}) kv
    , let (k, v) = BS.break (== 61 {- '=' -}) trimmed
    , not (BS.null v)
    ]

-- | 401 response with a @WWW-Authenticate: Basic@ challenge, for a
-- @\"required\"@ route with no (or invalid) credentials.
unauthorizedResponse :: Wai.Response
unauthorizedResponse =
    Wai.responseLBS
        status401
        [challenge, ("content-type", "text/plain; charset=utf-8")]
        "authentication required"
  where
    challenge :: Header
    challenge = ("WWW-Authenticate", "Basic realm=\"kitchen-sink\", charset=\"UTF-8\"")

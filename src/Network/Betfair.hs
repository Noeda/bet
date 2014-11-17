{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | API to Betfair (<http://www.betfair.com>).
--
-- You need a Betfair account, API keys and registered SSL certificates to use
-- this module.
--
-- Using the API may subject you to data charges:
--
-- <http://www.betfair.com/aboutUs/Betfair.Charges/>
--
-- By default this library will not let you do more than 4 requests per second.
-- This limit is global and is enforced even if you have multiple Betfair
-- connections.
--
-- This should stop you from being able to do anything that incurs data charges
-- by API use only (you can still ruin yourself by making bad bets), even the
-- heavily weighted charges.
--

module Network.Betfair
    (
    -- * Credentials
      Credentials(..)
    , HasCredentials(..)
    -- * Connection
    , openBetfair
    , closeBetfair
    , withOpenSSL
    -- * Types
    , module Network.Betfair.Types )
    where

import Control.Applicative
import Control.Concurrent
import Control.Lens hiding ( (.=) )
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Aeson
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text.Encoding as T
import Data.Typeable
import Network.Betfair.Internal
import Network.Betfair.Types
import Network.HTTP.Client.OpenSSL
import OpenSSL.Session
import Pipes
import Pipes.HTTP
import System.Mem.Weak

-- | Login credentials.
data Credentials = Credentials {
    _username :: Text                       -- ^ Your Betfair username.
  , _password :: Text                       -- ^ Your Betfair password.
  , _certificatePrivateKeyFile :: FilePath
  , _certificateCertificateFile :: FilePath
  , _apiKey :: Text }
  deriving ( Eq, Ord, Show, Read, Typeable )
makeClassy ''Credentials

data Work = Work !JsonRPCQuery !(MVar (Either SomeException BL.ByteString))
            deriving ( Eq, Typeable )

-- The type of all JSON-based queries sent out
data JsonRPCQuery = JsonRPCQuery
    { params :: !Value
    , methodName :: !Text }
    deriving ( Eq, Show, Typeable )

-- The type of all JSON-based queries received
newtype JsonRPC a = JsonRPC a

data BetfairHandle = BetfairHandle
    { _betfairThread :: !ThreadId
    , _workChannel :: !(MVar Work) }
makeClassy ''BetfairHandle

instance FromJSON a => FromJSON (JsonRPC a) where
    parseJSON (Object v) = JsonRPC <$> v .: "result"
    parseJSON _ = empty

instance ToJSON JsonRPCQuery where
    toJSON (JsonRPCQuery {..}) =
        object [ "jsonrpc" .= ("2.0" :: Text)
               , "method" .= ("SportsAPING/v1.0/" <> methodName :: Text)
               , "id" .= (1 :: Int)
               , "params" .= params ]

-- | A handle to a Betfair connection.
newtype Betfair = Betfair (MVar (Maybe BetfairHandle))
                  deriving ( Eq, Typeable )

-- | Connect to Betfair with given credentials.
--
-- The connection is cut when the returned `Betfair` value is garbage collected
-- or closed with `closeBetfair`.
--
-- Because Betfair connections are exclusively TLS -secured connections and
-- this library uses HsOpenSSL package, you need to have called `withOpenSSL`
-- from HsOpenSSL package somewhere in your program before you call this
-- (re-exported from "Network.Betfair" for convenience).
openBetfair :: MonadIO m => Credentials -> m Betfair
openBetfair credentials = liftIO $ mask_ $ do
    -- MVars. MVars everywhere.
    semaphore <- newEmptyMVar
    work_mvar <- newEmptyMVar
    weak_mvar_mvar <- newEmptyMVar

    bftid <- forkIOWithUnmask $ \unmask -> do
        weak_mvar <- takeMVar weak_mvar_mvar
        finally
            (unmask $ betfairConnection credentials semaphore work_mvar)
            (deRefWeak weak_mvar >>= \case
                Nothing -> return ()
                Just mvar -> modifyMVar_ mvar $ \_ -> return Nothing)

    let handle = BetfairHandle { _betfairThread = bftid
                               , _workChannel = work_mvar }

    mvar <- newEmptyMVar
    weak_mvar <- mkWeakMVar mvar $ closeBetfairHandle handle
    putMVar weak_mvar_mvar weak_mvar

    flip onException (killThread bftid) $
        takeMVar semaphore >>= \case
            Left err -> throwM err
            Right _ -> do putMVar mvar (Just handle)
                          return $ Betfair mvar

withBetfair :: Betfair -> (BetfairHandle -> IO a) -> IO a
withBetfair (Betfair mvar) action = withMVar mvar $ \case
    Nothing -> throwM BetfairIsClosed
    Just bhandle -> action bhandle

work :: (FromJSON a) => Betfair -> JsonRPCQuery -> IO a
work bf query = withBetfair bf $ \handle -> do
    result <- newEmptyMVar
    putMVar (_workChannel handle) $ Work query result
    takeMVar result >>= \case
        Left exc -> throwM exc
        Right result -> case decode result of
            Nothing -> throwM $ ParsingFailure "while trying to do work"
            Just (JsonRPC x) -> return x

betfairConnection :: Credentials
                  -> MVar (Either SomeException ())
                  -> MVar Work
                  -> IO ()
-- the pattern is lazy because if it's bottom, we catch it a too early
betfairConnection ~credentials@(Credentials{..}) semaphore work_mvar =
    flip catch (\(e :: SomeException) -> putMVar semaphore (Left e)) $ do
        -- establish SSL-enabled HTTP manager
        ssl_context <- context
        contextSetPrivateKeyFile ssl_context _certificatePrivateKeyFile
        contextSetCertificateFile ssl_context _certificateCertificateFile
        withManager (opensslManagerSettings $ return ssl_context) $ \m -> do
            session_key <- obtainSessionKey credentials m
            putMVar semaphore (Right ()) -- we are ready to process API calls
                                         -- so signal that to `openBetfair`.
            workLoop session_key credentials m work_mvar

workLoop :: SessionKey -> Credentials -> Manager -> MVar Work -> IO ()
workLoop session_key credentials m work_mvar = forever $ do
    work <- takeMVar work_mvar
    case work of
        Work sending result_mvar -> do
            flip onException (tryPutMVar result_mvar $ Left $
                              toException BetfairIsClosed) $ do
                -- --- IMPORTANT API LIMIT DO NOT TOUCH --- --
                workRateLimit -- make sure we don't make too many API calls per
                              -- second or we incur Betfair charges

                request <- betfairRequest session_key credentials sending
                withHTTP request m $ \response -> do
                    body <- readBodyLimited 50000000 response
                    putMVar result_mvar (Right body)

betfairRequest :: SessionKey -> Credentials -> JsonRPCQuery -> IO Request
betfairRequest session_key (Credentials{..}) query = do
    req <- parseUrl $ "https://api.betfair.com/exchange/betting/json-rpc/v1"
    return $ req { method = "POST"
                 , requestBody = RequestBodyBS $ BL.toStrict $ encode query
                 , requestHeaders = requestHeaders req <>
                    [ ("X-Application", T.encodeUtf8 _apiKey)
                    , ("X-Authentication", T.encodeUtf8 session_key)
                    , ("content-type", "application/json") ] }

obtainSessionKey :: Credentials
                 -> Manager
                 -> IO SessionKey
obtainSessionKey (Credentials{..}) m = do
    -- request to obtain a session key
    req' <- parseUrl "https://identitysso.betfair.com/api/certlogin"
    let req = urlEncodedBody [("username", T.encodeUtf8 _username)
                             ,("password", T.encodeUtf8 _password)] $
                req' { requestHeaders = requestHeaders req' <>
                        [ ("X-Application", T.encodeUtf8 _apiKey) ] }
    withHTTP req m $ \response -> do
        b <- readBodyLimited 1000000 response
        case decode b of
            Nothing -> throwM $ ParsingFailure "session key"
            Just login_response -> case login_response of
                LoginFailed reason -> throwM $ LoginFailure reason
                LoginSuccessful session_key -> return session_key

readBodyLimited :: Int
                -> Response (Producer ByteString IO ())
                -> IO BL.ByteString
readBodyLimited max_bytes response = do
    accumRef <- newIORef mempty
    runEffect $ responseBody response >-> accumulator accumRef 0
    BL.toLazyByteString <$> readIORef accumRef
  where
    accumulator accumRef builder_length = do
        block <- await
        accum <- liftIO $ readIORef accumRef
        when (B.length block + builder_length > max_bytes) $
            liftIO $ throwM TooMuchHTTPData
        liftIO $ writeIORef accumRef (accum <> BL.byteString block)
        accumulator accumRef (builder_length + B.length block)

-- | The type of exceptions that can happen with Betfair.
data BetfairException
  = TooMuchHTTPData
  -- ^ A HTTP response sent improbable amount of data.
  --   You are unlikely to see this unless Betfair
  --   is having a serious malfunction.
  | ParsingFailure Text
  -- ^ This exception is thrown if some data returned from Betfair
  --   failed to parse as expected JSON. It can mean a bug in our library
  --   or changes in the Betfair API that his library has not taken into
  --   account. The string has human-readable information.
  | LoginFailure Text
  -- ^ Login failed. The human-readable string tells why.
  | BetfairIsClosed
  -- ^ The `Betfair` handle you tried to use is closed.
  deriving ( Eq, Ord, Show, Read, Typeable )
instance Exception BetfairException

data CloseBetfair = CloseBetfair
                    deriving ( Eq, Ord, Show, Read, Typeable, Enum )
instance Exception CloseBetfair

-- | Close a `Betfair` connection promptly.
--
-- Does nothing if it was already closed. Trying to use `Betfair` after this
-- call will result in a user error.
closeBetfair :: MonadIO m => Betfair -> m ()
closeBetfair (Betfair mvar) = liftIO $ modifyMVar_ mvar $ \case
    Nothing -> return Nothing
    Just handle -> closeBetfairHandle handle >> return Nothing

closeBetfairHandle :: BetfairHandle -> IO ()
closeBetfairHandle (_betfairThread -> tid) = throwTo tid CloseBetfair


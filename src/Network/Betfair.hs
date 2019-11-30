{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
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
-- Use `request` to make calls to Betfair. Look at "Network.Betfair.Types" for
-- commands to use with `request`.
--
-- == Examples
--
-- This example opens a connection to Betfair and lists all your current bets.
--
-- @
--    {-\# LANGUAGE OverloadedStrings \#-}
--
--    import Network.Betfair
--
--    -- Use your actual credentials here. You need to see Betfair
--    -- documentation about that
--    credentials :: Credentials
--    credentials = {
--          _username = "Mr. Example"
--        , _password = "supersecret"
--        , _certificatePrivateKeyFile = "ssl/supersecret-client.key"
--        , _certificateCertificateFile = "ssl/supersecret-client.crt"
--        , _apiKey = "abcde12345"
--        }
--
--    main = do
--        bf <- openBetfair credentials
--        -- Many request types have a default (so you can use 'default'
--        -- function) but if they don't, you can check Network.Betfair.Types
--        -- module for the all the data types you can use in request.
--        results <- request (default { lcoOrderBy = Just ByMatchTime } :: ListCurrentOrders)
--        print results
--
-- @

module Network.Betfair
    (
    -- * Credentials
      Credentials(..)
    , HasCredentials(..)
    -- * Connection
    , openBetfair
    , closeBetfair
    , Betfair()
    -- * Operations
    , request
    , Network.Betfair.Internal.Request()
    -- * Types
    , module Network.Betfair.Types )
    where

import Control.Applicative
import Control.Concurrent
import Control.Lens hiding ( (.=) )
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Aeson
import Data.Binary hiding ( get, put, encode, decode )
import qualified Data.Binary as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Data
import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import Network.Betfair.Internal
import Network.Betfair.Types
import Network.HTTP.Client hiding ( Proxy, Request )
import qualified Network.HTTP.Client as PH
import Network.HTTP.Client.OpenSSL
import OpenSSL.Session
import System.Mem.Weak

-- | Login credentials.
data Credentials = Credentials {
    _username :: Text                       -- ^ Your Betfair username.
  , _password :: Text                       -- ^ Your Betfair password.
  , _certificatePrivateKeyFile :: FilePath
  , _certificateCertificateFile :: FilePath
  , _apiKey :: Text }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )
makeClassy ''Credentials

instance Binary Credentials where
    put (Credentials{..}) = do
        B.put (T.unpack _username)
        B.put (T.unpack _password)
        B.put _certificatePrivateKeyFile
        B.put _certificateCertificateFile
        B.put (T.unpack _apiKey)
    get = Credentials <$>
        (T.pack <$> B.get) <*>
        (T.pack <$> B.get) <*>
        B.get <*>
        B.get <*>
        (T.pack <$> B.get)

data Work = Work !Url !JsonRPCQuery !(MVar (Either SomeException BL.ByteString))
            deriving ( Eq, Typeable )

-- The type of all JSON-based queries sent out
data JsonRPCQuery = JsonRPCQuery
    { params :: !Value
    , methodName :: !Text }
    deriving ( Eq, Show, Typeable )

-- The type of all JSON-based queries received
newtype JsonRPC a = JsonRPC (Either APIException a)
  deriving ( Eq, Show, Typeable )

data BetfairHandle = BetfairHandle
    { _betfairThread :: !ThreadId
    , _workChannel :: !(MVar Work) }
makeClassy ''BetfairHandle

instance FromJSON a => FromJSON (JsonRPC a) where
    parseJSON (Object v) = JsonRPC <$>
        msum [ Right <$> v .: "result"
             , do err <- v .: "error"
                  dt <- err .: "data"
                  exc <- dt .: "APINGException"
                  details <- exc .: "errorDetails"
                  code <- exc .: "errorCode"
                  return $ Left $ APIException details code ]
    parseJSON _ = empty

instance ToJSON JsonRPCQuery where
    toJSON (JsonRPCQuery {..}) =
        object [ "jsonrpc" .= ("2.0" :: Text)
               , "method" .= methodName
               , "id" .= (1 :: Int)
               , "params" .= params ]

-- | A handle to a Betfair connection.
newtype Betfair = Betfair (MVar (Maybe BetfairHandle))
                  deriving ( Eq, Typeable )

-- | Connect to Betfair with given credentials.
--
-- The connection is cut when the returned `Betfair` value is garbage collected
-- or closed with `closeBetfair`.
openBetfair :: MonadIO m => Credentials -> m Betfair
openBetfair credentials = liftIO $ mask_ $ do
    -- MVars. MVars everywhere.
    semaphore <- newEmptyMVar
    work_mvar <- newEmptyMVar
    weak_mvar_mvar <- newEmptyMVar

    bftid <- forkIOWithUnmask $ \unmask ->
        -- catch `CloseBetfair` to suppress message
        flip catch (\CloseBetfair -> return ()) $ do
            withOpenSSL $ do
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

work :: forall a. (FromJSON a) => Betfair -> Url -> JsonRPCQuery -> IO a
work bf url query = withBetfair bf $ \handle -> do
    result <- newEmptyMVar
    putMVar (_workChannel handle) $ Work url query result
    takeMVar result >>= \case
        Left exc -> throwM exc
        Right result -> case eitherDecode result of
            Left err ->
              throwM $ ParsingFailure $ "Betfair behaved in unexpected way. Error: " <> T.pack err <> " Received raw string: " <> (T.pack $ show result)
            Right (JsonRPC (Left exc)) -> throwM exc
            Right (JsonRPC (Right x)) -> return x

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
            mask $ \restore -> do
                keep_alive_tid <- forkIO $ restore $ forever $ do
                                      -- wait 10 hours
                                      -- we use a loop because Int may be too
                                      -- small to hold the number of
                                      -- microseconds needed.
                                      replicateM_ (3600*10) $
                                          threadDelay 1000000
                                      keepAlive session_key credentials m
                finally (restore $ workLoop session_key credentials m work_mvar)
                        (killThread keep_alive_tid)

withResponseBody :: PH.Request -> Manager -> (B.ByteString -> IO a) -> IO a
withResponseBody request m action =
    withResponse request m $ \body_reader -> do
        let body = responseBody body_reader
        bs <- readUpToNBytes 50000000 body
        action bs
  where
    readUpToNBytes bytes body = recursive bytes body mempty
      where
        recursive 0 _ result = return result
        recursive n requestor result = do
            bs <- body
            if B.null bs
              then return result
              else recursive (n - B.length bs) requestor (result <> bs)

workLoop :: SessionKey -> Credentials -> Manager -> MVar Work -> IO ()
workLoop session_key credentials m work_mvar = forever $ do
    work <- takeMVar work_mvar
    case work of
        Work url sending result_mvar -> do
            flip onException (tryPutMVar result_mvar $ Left $
                              toException BetfairIsClosed) $ do
                -- --- IMPORTANT API LIMIT DO NOT TOUCH --- --
                workRateLimit -- make sure we don't make too many API calls per
                              -- second or we incur Betfair charges

                request <- betfairRequest session_key credentials sending url
                withResponseBody request m $ \body ->
                    putMVar result_mvar (Right $ BL.fromStrict body)

betfairRequest :: SessionKey
               -> Credentials
               -> JsonRPCQuery
               -> Url
               -> IO PH.Request
betfairRequest session_key (Credentials{..}) query url = do
    req <- parseUrl url
    return $ req { method = "POST"
                 , requestBody = RequestBodyBS $ BL.toStrict $ encode query
                 , requestHeaders = requestHeaders req <>
                    [ ("X-Application", T.encodeUtf8 _apiKey)
                    , ("X-Authentication", T.encodeUtf8 session_key)
                    , ("content-type", "application/json") ] }

keepAlive :: SessionKey -> Credentials -> Manager -> IO ()
keepAlive session_key (Credentials{..}) m = do
    req' <- parseUrl "https://identitysso-cert.betfair.com/api/keepAlive"
    let req = req' { requestHeaders = requestHeaders req' <>
                     [ ("Accept", "application/json")
                     , ("X-Application", T.encodeUtf8 _apiKey)
                     , ("X-Authentication", T.encodeUtf8 session_key) ] }
    withResponseBody req m $ \_ ->
        -- maybe it works, maybe it does not? I'm not sure what would be the
        -- sensible thing to do if keep-alive request fails. User probably
        -- still can continue to use the API as long as the session key is
        -- valid (which it will be for 12 hours).
        return ()

obtainSessionKey :: Credentials
                 -> Manager
                 -> IO SessionKey
obtainSessionKey (Credentials{..}) m = do
    -- request to obtain a session key
    req' <- parseUrl "https://identitysso-cert.betfair.com/api/certlogin"
    let req = urlEncodedBody [("username", T.encodeUtf8 _username)
                             ,("password", T.encodeUtf8 _password)] $
                req' { requestHeaders = requestHeaders req' <>
                        [ ("X-Application", T.encodeUtf8 _apiKey) ] }
    withResponseBody req m $ \b ->
        case decode $ BL.fromStrict b of
            Nothing -> throwM $ ParsingFailure "session key"
            Just login_response -> case login_response of
                LoginFailed reason -> throwM $ LoginFailure reason
                LoginSuccessful session_key -> return session_key

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

-- | Perform a request to `Betfair`.
request :: forall a b m. (MonadIO m, Network.Betfair.Internal.Request a b)
        => a -> Betfair -> m b
request req bf = liftIO $
    work bf (requestUrl (Proxy :: Proxy a))
            (JsonRPCQuery { methodName = requestMethod (Proxy :: Proxy a)
                          , params = toJSON req })

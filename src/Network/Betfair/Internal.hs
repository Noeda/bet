{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Internal operations of the Betfair API.
--

module Network.Betfair.Internal
    ( SessionKey
    , LoginResponse(..)
    , NumberOfRequests
    , rateLimit
    , workRateLimit
    , Request(..) )
    where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.IORef
import Data.Text ( Text )
import Data.Typeable
import System.IO.Unsafe

type SessionKey = Text

data LoginResponse = LoginFailed Text
                   | LoginSuccessful SessionKey
                   deriving ( Eq, Ord, Show, Read, Typeable )

instance FromJSON LoginResponse where
    parseJSON (Object v) =
        v .: "loginStatus" >>= \case
            "SUCCESS" -> LoginSuccessful <$> v .: "sessionToken"
            notsuccess -> return $ LoginFailed notsuccess
    parseJSON _ = empty

---- The rate limiter ----
type NumberOfRequests = Double

rateLimit :: IORef NumberOfRequests
rateLimit = unsafePerformIO $ newIORef 4
{-# NOINLINE rateLimit #-}

---- The lock used to limit the number of API calls ---

-- | Call before you request anything from Betfair, the locking system makes
-- sure this cannot be called too many times per second.
workRateLimit :: IO ()
workRateLimit = mask_ $ do
    modifyMVar_ workRateThreadMVar $ \case
        Nothing -> Just <$> forkIO workRateThread
        x -> return x
    takeMVar workRateLock

workRateLock :: MVar ()
workRateLock = unsafePerformIO $ newEmptyMVar
{-# NOINLINE workRateLock #-}

workRateThreadMVar :: MVar (Maybe ThreadId)
workRateThreadMVar = unsafePerformIO $ newMVar Nothing
{-# NOINLINE workRateThread #-}

workRateThread :: IO ()
workRateThread = forever $ do
    putMVar workRateLock ()
    x <- readIORef rateLimit
    if x <= 0
      then forever $ threadDelay 10000000
      else when (x > 0) $
               threadDelay $ ceiling $ (1000000 :: Double) / x

-- | Class of types that can be used as requests to Betfair, with their
-- response defined as the second type variable.
class (ToJSON a, FromJSON b) => Request a b | a -> b where
    requestMethod :: Proxy a -> Text


-- | (Financially) unsafe Betfair API functions.
--

module Network.Betfair.Unsafe
    (
    -- * Rate limiting
      setAPIRateLimit
    , getAPIRateLimit
    , NumberOfRequests )
    where

import Data.IORef
import Network.Betfair.Internal

-- | Set the global rate limit.
--
-- The initial value is 4. The rate limiting is global and shared with all
-- Betfair connections in the same Haskell process.
--
-- DANGEROUS! If the limit allows for more than 20 requests per second then you
-- have to pay up Betfair data charge fee.
--
-- This can be a fractional value like 19.8 if you like living dangerously.
--
-- Setting this to 0 prevents all further communication with Betfair (aside
-- from logging in).
setAPIRateLimit :: NumberOfRequests -> IO ()
setAPIRateLimit nor
    | nor < 0 = error "setAPIRateLimit: limit cannot be negative."
    | otherwise = atomicModifyIORef' rateLimit $ \_ -> ( nor, () )

-- | Returns the current global rate limit.
getAPIRateLimit :: IO Double
getAPIRateLimit = readIORef rateLimit


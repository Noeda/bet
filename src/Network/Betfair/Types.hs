{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines most data types in the betting API of Betfair API.
--
-- Where possible, data types are in 1:1 correspondence to the ones documented
-- in the API.
--
-- All types have lenses where they can.
--

module Network.Betfair.Types
    ( ListMarketCatalogue(..)
    , HasListMarketCatalogue(..)
    , MarketFilter(..)
    , HasMarketFilter(..)
    -- * IDs
    , EventTypeId(..)
    , EventId(..)
    , Country(..)
    , MarketTypeCode(..)
    , CompetitionId(..)
    , MarketId(..)
    , Venue(..)
    -- * Enums
    , MarketSort(..)
    , MarketProjection(..) )
    where

import Control.Lens
import Data.Aeson
import Data.Text ( Text )
import Data.Time
import Data.Typeable
import qualified Data.Set as S
import Prelude hiding ( filter )

newtype EventTypeId = EventTypeId { getEventTypeId :: Text }
                      deriving ( Eq, Ord, Show, Read, Typeable
                               , FromJSON, ToJSON )

newtype EventId = EventId { getEventId :: Text }
                  deriving ( Eq, Ord, Show, Read, Typeable
                           , FromJSON, ToJSON )

newtype Country = Country { getCountry :: Text }
                  deriving ( Eq, Ord, Show, Read, Typeable
                           , FromJSON, ToJSON )

newtype MarketTypeCode = MarketTypeCode { getMarketTypeCode :: Text }
                         deriving ( Eq, Ord, Show, Read, Typeable
                                  , FromJSON, ToJSON )

newtype CompetitionId = CompetitionId { getCompetitionId :: Text }
                        deriving ( Eq, Ord, Show, Read, Typeable
                                 , FromJSON, ToJSON )

newtype MarketId = MarketId { getMarketId :: Text }
                   deriving ( Eq, Ord, Show, Read, Typeable
                            , FromJSON, ToJSON )

newtype Venue = Venue { getVenue :: Text }
                deriving ( Eq, Ord, Show, Read, Typeable
                         , FromJSON, ToJSON )

data ListMarketCatalogue = ListMarketCatalogue
    { _filter :: MarketFilter
    , _marketProjection :: Maybe (S.Set MarketProjection)
    , _sort :: Maybe (S.Set MarketSort)
    , _maxResults :: Int
    , _locale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketSort
    = MinimumTraded
    | MaximumTraded
    | MinimumAvailable
    | MaximumAvailable
    | FirstToStart
    | LastToStart
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data MarketProjection
    = Competition
    | Event
    | EventType
    | MarketStartTime
    | MarketDescription
    | RunnerDescription
    | RunnerMetadata
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data MarketBettingType
    = Odds
    | Line
    | Range
    | AsianHandicapDoubleLine
    | AsianHandicapSingleLine
    | FixedOdds
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data OrderStatus
    = ExecutionComplete
    | Executable
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data MarketFilter = MarketFilter
    { _textQuery :: Maybe String
    , _eventTypeIds :: Maybe (S.Set EventTypeId)
    , _eventIds :: Maybe (S.Set EventId)
    , _competitionIds :: Maybe (S.Set CompetitionId)
    , _marketIds :: Maybe (S.Set MarketId)
    , _venues :: Maybe (S.Set Venue)
    , _bspOnly :: Maybe Bool
    , _turnInPlayEnabled :: Maybe Bool
    , _inPlayOnly :: Maybe Bool
    , _marketBettingTypes :: Maybe (S.Set MarketBettingType)
    , _marketCountries :: Maybe (S.Set Country)
    , _marketTypeCodes :: Maybe (S.Set MarketTypeCode)
    , _marketStartTime :: Maybe (UTCTime, UTCTime)
    , _withOrders :: Maybe (S.Set OrderStatus) }
    deriving ( Eq, Ord, Show, Read, Typeable )

makeClassy ''ListMarketCatalogue
makeClassy ''MarketFilter


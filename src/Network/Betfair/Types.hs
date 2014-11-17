{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines most data types in the betting API of Betfair API,
-- corresponding to version 2.0.
--
-- Where possible, data types are in 1:1 correspondence to the ones documented
-- in the API. This is not always possible because there are name clashes. So
-- if you see spurious letters after some names then that's name clashes being
-- resolved the ugly way. (Maybe in the future we'll use
-- OverloadedRecordFields?).
--
-- All types have lenses where they can.
--

module Network.Betfair.Types
    ( ListMarketCatalogue(..)
    , HasListMarketCatalogue(..)
    , MarketFilter(..)
    , HasMarketFilter(..)
    -- * IDs
    , Country(..)
    , CompetitionId(..)
    , EventId(..)
    , EventTypeId(..)
    , MarketId(..)
    , MarketTypeCode(..)
    , SelectionId(..)
    , Venue(..)
    -- * Enums
    , MarketBettingType(..)
    , MarketProjection(..)
    , MarketSort(..)
    , OrderStatus(..) )
    where

import Control.Lens
import Data.Aeson
import Data.Text ( Text )
import Data.Time
import Data.Typeable
import qualified Data.Set as S
import qualified Data.Map.Lazy as M
import Prelude hiding ( filter, id )

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

newtype SelectionId = SelectionId { getSelectionId :: Text }
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
    | MarketDescriptionE
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

data MarketCatalogue = MarketCatalogue
    { _marketId :: MarketId
    , _marketName :: Text
    , _mcmarketStartTime :: Maybe UTCTime
    , _description :: Maybe MarketDescription
    , _totalMatched :: Maybe Double
    , _runners :: Maybe [RunnerCatalog]
    , _eventType :: Maybe EventTypeC
    , _competition :: Maybe Competition
    , _event :: Maybe Event }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketDescription = MarketDescription
    { _persistenceEnabled :: Bool
    , _bspMarket :: Bool
    , _marketTime :: UTCTime
    , _suspendTime :: UTCTime
    , _settleTime :: Maybe UTCTime
    , _bettingType :: MarketBettingType
    , _mdturnInPlayEnabled :: Bool
    , _marketType :: Text
    , _regulator :: Text
    , _marketBaseRate :: Double
    , _discountAllowed :: Bool
    , _wallet :: Maybe Text
    , _rules :: Maybe Text
    , _rulesHasDate :: Maybe Bool
    , _clarifications :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data EventTypeC = EventTypeC
    { _id :: EventTypeId
    , _name :: Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Competition = CompetitionC
    { _cid :: CompetitionId
    , _cname :: Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Event = EventC
    { _eid :: EventId
    , _ename :: Text
    , _countryCode :: Country
    , _timezone :: Text
    , _venue :: Text
    , _openDate :: UTCTime }
    deriving ( Eq, Ord, Show, Read, Typeable )

data RunnerCatalog = RunnerCatalog
    { _selectionId :: SelectionId
    , _runnerName :: Text
    , _handicap :: Double
    , _sortPriority :: Int
    , _metadata :: M.Map Text Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

makeClassy ''ListMarketCatalogue
makeClassy ''MarketFilter
makeClassy ''MarketCatalogue
makeClassy ''MarketDescription
makeClassy ''RunnerCatalog
makeClassy ''EventTypeC


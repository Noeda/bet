{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines most data types in the betting API of Betfair API,
-- corresponding to version 2.0.
--
-- Where possible, data types are in 1:1 correspondence to the ones documented
-- in the API. The exceptions are ID-like values and some types that can be
-- found in the "Data.Bet" module (that are isomorphic to the raw types).
-- However, because there are name clashes, all record names are prefixed with
-- the two-letter abbreviation of the data type they are defined in. All
-- recordful constructors have C at their end.
--
-- All types have lenses where they can.
--

module Network.Betfair.Types
    (
    -- * Data Types
      Competition(..)
    , ExBestOffersOverrides(..)
    , ExchangePrices(..)
    , Event(..)
    , EventType(..)
    , ListMarketBook(..)
    , ListMarketCatalogue(..)
    , MarketBook(..)
    , MarketCatalogue(..)
    , MarketDescription(..)
    , MarketFilter(..)
    , Match(..)
    , Order(..)
    , PriceProjection(..)
    , PriceSize(..)
    , Runner(..)
    , RunnerCatalog(..)
    , StartingPrices(..)
    -- ** Classy
    , HasCompetition(..)
    , HasExBestOffersOverrides(..)
    , HasExchangePrices(..)
    , HasEvent(..)
    , HasEventType(..)
    , HasListMarketBook(..)
    , HasListMarketCatalogue(..)
    , HasMarketBook(..)
    , HasMarketCatalogue(..)
    , HasMarketDescription(..)
    , HasMarketFilter(..)
    , HasMatch(..)
    , HasOrder(..)
    , HasPriceProjection(..)
    , HasPriceSize(..)
    , HasRunner(..)
    , HasRunnerCatalog(..)
    , HasStartingPrices(..)
    -- * IDs
    , BetId(..)
    , Country(..)
    , CompetitionId(..)
    , EventId(..)
    , EventTypeId(..)
    , MarketId(..)
    , MarketTypeCode(..)
    , MatchId(..)
    , SelectionId(..)
    , Venue(..)
    -- * Enums
    , MarketBettingType(..)
    , MarketProjection(..)
    , MarketSort(..)
    , MarketStatus(..)
    , MatchProjection(..)
    , OrderProjection(..)
    , OrderStatus(..)
    , OrderType(..)
    , PersistenceType(..)
    , PriceData(..)
    , RollupModel(..)
    , RunnerStatus(..)
    , Side )
    where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Bet
import Data.Char
import Data.Text ( Text )
import Data.Time
import Data.Typeable
import qualified Data.Set as S
import qualified Data.Map.Lazy as M
import Network.Betfair.Types.TH
import Prelude hiding ( filter, id )

newtype BetId = BetId { getBetId :: Text }
                deriving ( Eq, Ord, Show, Read, Typeable, FromJSON, ToJSON )

newtype Country = Country { getCountry :: Text }
                  deriving ( Eq, Ord, Show, Read, Typeable
                           , FromJSON, ToJSON )

newtype CompetitionId = CompetitionId { getCompetitionId :: Text }
                        deriving ( Eq, Ord, Show, Read, Typeable
                                 , FromJSON, ToJSON )

newtype EventId = EventId { getEventId :: Text }
                  deriving ( Eq, Ord, Show, Read, Typeable
                           , FromJSON, ToJSON )

newtype EventTypeId = EventTypeId { getEventTypeId :: Text }
                      deriving ( Eq, Ord, Show, Read, Typeable
                               , FromJSON, ToJSON )

newtype MarketId = MarketId { getMarketId :: Text }
                   deriving ( Eq, Ord, Show, Read, Typeable
                            , FromJSON, ToJSON )

newtype MarketTypeCode = MarketTypeCode { getMarketTypeCode :: Text }
                         deriving ( Eq, Ord, Show, Read, Typeable
                                  , FromJSON, ToJSON )

newtype MatchId = MatchId { getMatchId :: Text }
                  deriving ( Eq, Ord, Show, Read, Typeable, FromJSON, ToJSON )

newtype SelectionId = SelectionId { getSelectionId :: Int }
                      deriving ( Eq, Ord, Show, Read, Typeable
                               , FromJSON, ToJSON )

newtype Venue = Venue { getVenue :: Text }
                deriving ( Eq, Ord, Show, Read, Typeable
                         , FromJSON, ToJSON )

data Competition = CompetitionC
    { _cid :: CompetitionId
    , _cname :: Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ExBestOffersOverrides = ExBestOffersOverridesC
    { _ebbestPricesDepth :: Maybe Int
    , _ebrollupModel :: Maybe RollupModel
    , _ebrollupLimit :: Maybe Int
    , _ebrollupLiabilityThreshold :: Maybe Double
    , _ebrollupLiabilityFactor :: Maybe Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ExchangePrices = ExchangePricesC
    { _epavailableToBack :: Maybe [PriceSize]
    , _epavailableToLay :: Maybe [PriceSize]
    , _eptradedVolume :: Maybe [PriceSize] }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Event = EventC
    { _eid :: EventId
    , _ename :: Text
    , _ecountryCode :: Country
    , _etimezone :: Text
    , _evenue :: Text
    , _eopenDate :: UTCTime }
    deriving ( Eq, Ord, Show, Read, Typeable )

data EventType = EventTypeC
    { _etid :: EventTypeId
    , _etname :: Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListMarketBook = ListMarketBookC
    { _lmbmarketIds :: [MarketId]
    , _lmbpriceProjection :: Maybe PriceProjection
    , _lmborderProjection :: Maybe OrderProjection
    , _lmbmatchProjection :: Maybe MatchProjection
    , _lmbcurrencyCode :: Maybe Text
    , _lmblocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListMarketCatalogue = ListMarketCatalogueC
    { _lmcfilter :: MarketFilter
    , _lmcmarketProjection :: Maybe (S.Set MarketProjection)
    , _lmcsort :: Maybe (S.Set MarketSort)
    , _lmcmaxResults :: Int
    , _lmclocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketBook = MarketBookC
    { _mbmarketId :: MarketId
    , _mbisMarketDataDelayed :: Bool
    , _mbstatus :: Maybe MarketStatus
    , _mbbetDelay :: Maybe Int
    , _mbbspReconciled :: Maybe Bool
    , _mbcomplete :: Maybe Bool
    , _mbinplay :: Maybe Bool
    , _mbnumberOfWinners :: Maybe Int
    , _mbnumberOfRunners :: Maybe Int
    , _mbnumberOfActiveRunners :: Maybe Int
    , _mblastMatchTime :: Maybe UTCTime
    , _mbtotalMatched :: Maybe Double
    , _mbtotalAvailable :: Maybe Double
    , _mbcrossMatching :: Maybe Bool
    , _mbrunnersVoidable :: Maybe Bool
    , _mbversion :: Maybe Int
    , _mbrunners :: [Runner] }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketCatalogue = MarketCatalogueC
    { _mcmarketId :: MarketId
    , _mcmarketName :: Text
    , _mcmarketStartTime :: Maybe UTCTime
    , _mcdescription :: Maybe MarketDescription
    , _mctotalMatched :: Maybe Double
    , _mcrunners :: Maybe [RunnerCatalog]
    , _mceventType :: Maybe EventType
    , _mccompetition :: Maybe Competition
    , _mcevent :: Maybe Event }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketDescription = MarketDescriptionC
    { _mdpersistenceEnabled :: Bool
    , _mdbspMarket :: Bool
    , _mdmarketTime :: UTCTime
    , _mdsuspendTime :: UTCTime
    , _mdsettleTime :: Maybe UTCTime
    , _mdbettingType :: MarketBettingType
    , _mdturnInPlayEnabled :: Bool
    , _mdmarketType :: Text
    , _mdregulator :: Text
    , _mdmarketBaseRate :: Double
    , _mddiscountAllowed :: Bool
    , _mdwallet :: Maybe Text
    , _mdrules :: Maybe Text
    , _mdrulesHasDate :: Maybe Bool
    , _mdclarifications :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketFilter = MarketFilterC
    { _mftextQuery :: Maybe String
    , _mfeventTypeIds :: Maybe (S.Set EventTypeId)
    , _mfeventIds :: Maybe (S.Set EventId)
    , _mfcompetitionIds :: Maybe (S.Set CompetitionId)
    , _mfmarketIds :: Maybe (S.Set MarketId)
    , _mfvenues :: Maybe (S.Set Venue)
    , _mfbspOnly :: Maybe Bool
    , _mfturnInPlayEnabled :: Maybe Bool
    , _mfinPlayOnly :: Maybe Bool
    , _mfmarketBettingTypes :: Maybe (S.Set MarketBettingType)
    , _mfmarketCountries :: Maybe (S.Set Country)
    , _mfmarketTypeCodes :: Maybe (S.Set MarketTypeCode)
    , _mfmarketStartTime :: Maybe (UTCTime, UTCTime)
    , _mfwithOrders :: Maybe (S.Set OrderStatus) }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Match = MatchC
    { _mbetId :: Maybe BetId
    , _mmatchId :: Maybe MatchId
    , _mside :: Side
    , _mprice :: Double
    , _msize :: Double
    , _mmatchDate :: Maybe UTCTime }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Order = OrderC
    { _obetId :: BetId
    , _oorderType :: OrderType
    , _ostatus :: OrderStatus
    , _opersistenceType :: PersistenceType
    , _oside :: Side
    , _oprice :: Double
    , _osize :: Double
    , _obspLiability :: Double
    , _oplacedDate :: UTCTime
    , _oavgPriceMatched :: Maybe Double
    , _osizeMatched :: Maybe Double
    , _osizeRemaining :: Maybe Double
    , _osizeLapsed :: Maybe Double
    , _osizeCancelled :: Maybe Double
    , _osizeVoided :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data PriceProjection = PriceProjectionC
    { _pppriceData :: Maybe (S.Set PriceData)
    , _ppexBestOffersOverrides :: Maybe ExBestOffersOverrides
    , _ppvirtualize :: Maybe Bool
    , _pprolloverStakes :: Maybe Bool }
    deriving ( Eq, Ord, Show, Read, Typeable )

data PriceSize = PriceSizeC
    { _psprice :: !Double
    , _pssize :: !Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Runner = RunnerC
    { _rselectionId :: SelectionId
    , _rhandicap :: Double
    , _rstatus :: RunnerStatus
    , _radjustmentFactor :: Double
    , _rlastPriceTraded :: Maybe Double
    , _rtotalMatched :: Maybe Double
    , _rremovalData :: Maybe UTCTime
    , _rsp :: Maybe StartingPrices
    , _rex :: Maybe ExchangePrices
    , _rorders :: Maybe [Order]
    , _rmatches :: Maybe [Match] }
    deriving ( Eq, Ord, Show, Read, Typeable )

data RunnerCatalog = RunnerCatalogC
    { _rcselectionId :: SelectionId
    , _rcrunnerName :: Text
    , _rchandicap :: Double
    , _rcsortPriority :: Int
    , _rcmetadata :: M.Map Text Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data StartingPrices = StartingPricesC
    { _spnearPrice :: Maybe Double
    , _spfarPrice :: Maybe Double
    , _spbackStakeTaken :: Maybe [PriceSize]
    , _splayLiabilityTaken :: Maybe [PriceSize]
    , _spactualSP :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketBettingType
    = Odds
    | Line
    | Range
    | AsianHandicapDoubleLine
    | AsianHandicapSingleLine
    | FixedOdds
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

data MarketSort
    = MinimumTraded
    | MaximumTraded
    | MinimumAvailable
    | MaximumAvailable
    | FirstToStart
    | LastToStart
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data MarketStatus
    = Inactive
    | Open
    | Suspended
    | Closed
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data MatchProjection
    = NoRollup
    | RolledUpByPrice
    | RolledUpByAvgPrice
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data OrderProjection
    = OpAll
    | OpExecutable
    | OpExecutionComplete
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data OrderStatus
    = ExecutionComplete
    | Executable
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data OrderType
    = Limit
    | LimitOnClose
    | MarketOnClose
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data PersistenceType
    = PtLapse
    | PtPersist
    | PtMarketOnClose
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data PriceData
    = SpAvailable
    | SpTraded
    | ExBestOffers
    | ExAllOffers
    | ExTraded
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data RollupModel
    = Stake
    | Payout
    | ManagedLiability
    | None
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data RunnerStatus
    = Active
    | Winner
    | Loser
    | RemovedVacant
    | Removed
    | Hidden
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

type Side = BetType

-- keep in alphabetical order so that it's easy to scan with the definitions
-- above
makeClassy ''Competition
makeClassy ''ExBestOffersOverrides
makeClassy ''ExchangePrices
makeClassy ''Event
makeClassy ''EventType
makeClassy ''ListMarketBook
makeClassy ''ListMarketCatalogue
makeClassy ''MarketBook
makeClassy ''MarketCatalogue
makeClassy ''MarketDescription
makeClassy ''MarketFilter
makeClassy ''Match
makeClassy ''Order
makeClassy ''PriceProjection
makeClassy ''PriceSize
makeClassy ''Runner
makeClassy ''RunnerCatalog
makeClassy ''StartingPrices

instance FromJSON BetType where
    parseJSON (String "BACK") = pure Back
    parseJSON (String "LAY") = pure Lay
    parseJSON _ = empty

instance ToJSON BetType where
    toJSON Back = String "BACK"
    toJSON Lay = String "LAY"

-- the number is how many characters to drop from a record field name to get
-- the JSON name in the API
deriveJSON (commonStruct 2) ''Competition
deriveJSON (commonStruct 3) ''ExBestOffersOverrides
deriveJSON (commonStruct 3) ''ExchangePrices
deriveJSON (commonStruct 2) ''Event
deriveJSON (commonStruct 3) ''EventType
deriveJSON (commonStruct 4) ''ListMarketBook
deriveJSON (commonStruct 4) ''ListMarketCatalogue
deriveJSON (commonStruct 3) ''MarketBook
deriveJSON (commonStruct 3) ''MarketCatalogue
deriveJSON (commonStruct 3) ''MarketDescription
deriveJSON (commonStruct 3) ''MarketFilter
deriveJSON (commonStruct 2) ''Match
deriveJSON (commonStruct 2) ''Order
deriveJSON (commonStruct 3) ''PriceProjection
deriveJSON (commonStruct 3) ''PriceSize
deriveJSON (commonStruct 2) ''Runner
deriveJSON (commonStruct 3) ''RunnerCatalog
deriveJSON (commonStruct 3) ''StartingPrices

deriveJSON (commonEnum 0) ''MarketBettingType
deriveJSON (commonEnum 0) ''MarketProjection
deriveJSON (commonEnum 0) ''MarketSort
deriveJSON (commonEnum 0) ''MarketStatus
deriveJSON (commonEnum 0) ''MatchProjection
deriveJSON (commonEnum 2) ''OrderProjection
deriveJSON (commonEnum 0) ''OrderStatus
deriveJSON (commonEnum 0) ''OrderType
deriveJSON (commonEnum 2) ''PersistenceType
deriveJSON (commonEnum 0) ''PriceData
deriveJSON (commonEnum 0) ''RollupModel
deriveJSON (commonEnum 0) ''RunnerStatus


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines most data types in the betting API of Betfair API,
-- corresponding to version 2.0. Refer to Betfair API documentation for
-- meanings of the values.
--
-- <https://developer.betfair.com/default/api-s-and-services/sports-api/sports-overview/>
--
-- Where possible, data types are in 1:1 correspondence to the ones documented
-- in the API. The exceptions are ID-like values and some types that can be
-- found in the "Data.Bet" module (that are isomorphic to the raw types).
-- However, because there are name clashes, all record names are prefixed with
-- the two-letter abbreviation of the data type they are defined in. All
-- recordful constructors have C at their end.
--
-- Many of the records have lenses created with `makeClassy` from the lens
-- package.
--
-- A future version of this module might use overloaded records of some kind to
-- avoid bazillion different names in record fields. Stay tuned.
--
-- As of writing of this, the whole surface area of these types have not been
-- tested. If your Betfair requests fail for mysterious reasons, it's possible
-- a field is not a `Maybe` field when it should or there is a typo somewhere.
-- Betfair's documentation does say which fields should be present but the
-- information seems to be inaccurate; leading us to speculate how exactly
-- these values work.
--

module Network.Betfair.Types
    (
    -- * Data Types
      ClearedOrderSummary(..)
    , ClearedOrderSummaryReport(..)
    , Competition(..)
    , CompetitionResult(..)
    , CountryCodeResult(..)
    , CurrentOrderSummary(..)
    , CurrentOrderSummaryReport(..)
    , ExBestOffersOverrides(..)
    , ExchangePrices(..)
    , Event(..)
    , EventResult(..)
    , EventType(..)
    , EventTypeResult(..)
    , ItemDescription(..)
    , ListCompetitions(..)
    , ListCountries(..)
    , ListCurrentOrders(..)
    , ListClearedOrders(..)
    , ListEvents(..)
    , ListEventTypes(..)
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
    , RunnerId(..)
    , StartingPrices(..)
    , TimeRange(..)
    -- ** Requests
    , Request()
    -- ** Defaults
    , Default(..)
    -- ** Classy
    , HasClearedOrderSummary(..)
    , HasClearedOrderSummaryReport(..)
    , HasCompetition(..)
    , HasCompetitionResult(..)
    , HasCountryCodeResult(..)
    , HasCurrentOrderSummary(..)
    , HasCurrentOrderSummaryReport(..)
    , HasExBestOffersOverrides(..)
    , HasExchangePrices(..)
    , HasEvent(..)
    , HasEventResult(..)
    , HasEventType(..)
    , HasEventTypeResult(..)
    , HasItemDescription(..)
    , HasListCompetitions(..)
    , HasListCountries(..)
    , HasListCurrentOrders(..)
    , HasListClearedOrders(..)
    , HasListEvents(..)
    , HasListEventTypes(..)
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
    , HasRunnerId(..)
    , HasStartingPrices(..)
    , HasTimeRange(..)
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
    , BetStatus(..)
    , GroupBy(..)
    , MarketBettingType(..)
    , MarketProjection(..)
    , MarketSort(..)
    , MarketStatus(..)
    , MatchProjection(..)
    , OrderBy(..)
    , OrderProjection(..)
    , OrderStatus(..)
    , OrderType(..)
    , PersistenceType(..)
    , PriceData(..)
    , RollupModel(..)
    , RunnerStatus(..)
    , Side
    , SortDir(..)
    -- * Exceptions
    , APIException(..)
    , APIExceptionCode(..)
    , pattern APIExcCode )
    where

import Control.Monad.Catch
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Bet
import Data.Text ( Text )
import Data.Time
import Data.Typeable
import qualified Data.Set as S
import qualified Data.Map.Lazy as M
import Network.Betfair.Internal
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

data ClearedOrderSummary = ClearedOrderSummaryC
    { _coeventTypeId :: EventTypeId
    , _coeventId :: EventId
    , _comarketId :: MarketId
    , _coselectionId :: SelectionId
    , _cohandicap :: Double
    , _cobetId :: BetId
    , _coplacedDate :: UTCTime
    , _copersistenceType :: Maybe PersistenceType
    , _coorderType :: Maybe OrderType
    , _coside :: Maybe Side
    , _coitemDescription :: Maybe ItemDescription
    , _copriceRequested :: Maybe Double
    , _cosettledDate :: Maybe UTCTime
    , _cobetCount :: Int
    , _cocommission :: Maybe Double
    , _copriceMatched :: Maybe Double
    , _copriceReduced :: Maybe Bool
    , _cosizeSettled :: Maybe Double
    , _coprofit :: Double
    , _cosizeCancelled :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Competition = CompetitionC
    { _cid :: CompetitionId
    , _cname :: Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data CompetitionResult = CompetitionResultC
    { _ccompetition :: Competition
    , _cmarketCount :: Int
    , _ccompetitionRegion :: Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data CountryCodeResult = CountryCodeResultC
    { _cccountryCode :: Text
    , _ccmarketCount :: Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ClearedOrderSummaryReport = ClearedOrderSummaryReportC
    { _coclearedOrders :: [ClearedOrderSummary]
    , _comoreAvailable :: Bool }
    deriving ( Eq, Ord, Show, Read, Typeable )

data CurrentOrderSummary = CurrentOrderSummaryC
    { _cbetId :: BetId
    , _cmarketId :: MarketId
    , _cselectionId :: SelectionId
    , _chandicap :: Double
    , _cpriceSize :: PriceSize
    , _cbspLiability :: Double
    , _cside :: Side
    , _cstatus :: OrderStatus
    , _cpersistenceType :: PersistenceType
    , _corderType :: OrderType
    , _cplacedDate :: UTCTime
    , _cmatchedDate :: Maybe UTCTime
    , _caveragePriceMatched :: Maybe Double
    , _csizeMatched :: Maybe Double
    , _csizeRemaining :: Maybe Double
    , _csizeLapsed :: Maybe Double
    , _csizeCancelled :: Maybe Double
    , _csizeVoided :: Maybe Double
    , _cregulatorAuthCode :: Maybe Text
    , _cregulatorCode :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data CurrentOrderSummaryReport = CurrentOrderSummaryReportC
    { _ccurrentOrders :: [CurrentOrderSummary]
    , _cmoreAvailable :: Bool }
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
    , _ecountryCode :: Maybe Country
    , _etimezone :: Text
    , _evenue :: Maybe Text
    , _eopenDate :: UTCTime }
    deriving ( Eq, Ord, Show, Read, Typeable )

data EventResult = EventResultC
    { _erevent :: Event
    , _ermarketCount :: Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data EventType = EventTypeC
    { _etid :: EventTypeId
    , _etname :: Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data EventTypeResult = EventTypeResultC
    { _etreventType :: EventType
    , _etrmarketCount :: Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ItemDescription = ItemDescriptionC
    { _ieventTypeDesc :: Maybe Text
    , _ieventDesc :: Maybe Text
    , _imarketDesc :: Maybe Text
    , _imarketStartTime :: Maybe UTCTime
    , _irunnerDesc :: Maybe Text
    , _inumberOfWinners :: Maybe Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListCompetitions = ListCompetitionsC
    { _lcfilter :: MarketFilter
    , _lclocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListCountries = ListCountriesC
    { _lcsfilter :: MarketFilter
    , _lcslocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListCurrentOrders = ListCurrentOrdersC
    { _lcobetIds :: Maybe (S.Set BetId)
    , _lcomarketIds :: Maybe (S.Set MarketId)
    , _lcoorderProjection :: Maybe OrderProjection
    , _lcodateRange :: Maybe TimeRange
    , _lcoorderBy :: Maybe OrderBy
    , _lcosortDir :: Maybe SortDir
    , _lcofromRecord :: Maybe Int
    , _lcorecordCount :: Maybe Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListClearedOrders = ListClearedOrdersC
    { _lcrbetStatus :: BetStatus
    , _lcreventTypeIds :: Maybe (S.Set EventTypeId)
    , _lcreventIds :: Maybe (S.Set EventId)
    , _lcrmarketIds :: Maybe (S.Set MarketId)
    , _lcrrunnerIds :: Maybe (S.Set RunnerId)
    , _lcrbetIds :: Maybe (S.Set BetId)
    , _lcrside :: Maybe Side
    , _lcrsettledDateRange :: Maybe TimeRange
    , _lcrgroupBy :: Maybe GroupBy
    , _lcrincludeItemDescription :: Maybe Bool
    , _lcrlocale :: Maybe Text
    , _lcrfromRecord :: Maybe Int
    , _lcrrecordCount :: Maybe Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListEvents = ListEventsC
    { _lefilter :: MarketFilter
    , _lelocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListEventTypes = ListEventTypesC
    { _letfilter :: MarketFilter
    , _letlocale :: Maybe Text }
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
    , _radjustmentFactor :: Maybe Double -- the official documentation says
                                         -- this is required but it's lying.
                                         -- lying! so we use Maybe
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

data RunnerId = RunnerIdC
    { _rimarketId :: MarketId
    , _riselectionId :: SelectionId
    , _rihandicap :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data StartingPrices = StartingPricesC
    { _spnearPrice :: Maybe Double
    , _spfarPrice :: Maybe Double
    , _spbackStakeTaken :: Maybe [PriceSize]
    , _splayLiabilityTaken :: Maybe [PriceSize]
    , _spactualSP :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data TimeRange = TimeRangeC
    { _tfrom :: UTCTime
    , _tto :: UTCTime }
    deriving ( Eq, Ord, Show, Read, Typeable )

data BetStatus
    = Settled
    | Voided
    | Lapsed
    | Cancelled
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data GroupBy
    = GBEventType
    | GBEvent
    | GBMarket
    | GBSide
    | GBBet
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

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

data OrderBy
    = ByBet
    | ByMarket
    | ByMatchTime
    | ByPlaceTime
    | BySettledTime
    | ByVoidTime
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

data SortDir
    = EarliestToLatest
    | LatestToEarliest
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data APIException = APIException { exceptionDetails :: Text
                                 , exceptionCode :: APIExceptionCode }
                    deriving ( Eq, Ord, Show, Read, Typeable )

instance Exception APIException

-- | Pattern synonym that just matches on the `APIExceptionCode` part of
-- `APIException`.
pattern APIExcCode b <- APIException _ b

data APIExceptionCode
    = TooMuchData
    | InvalidInputData
    | InvalidSessionInformation
    | NoAppKey
    | NoSession
    | UnexpectedError
    | InvalidAppKey
    | TooManyRequests
    | ServiceBusy
    | TimeoutError
    | RequestSizeExceedsLimit
    | AccessDenied
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

-- keep in alphabetical order so that it's easy to scan with the definitions
-- above
makeClassy ''ClearedOrderSummary
makeClassy ''ClearedOrderSummaryReport
makeClassy ''Competition
makeClassy ''CompetitionResult
makeClassy ''CountryCodeResult
makeClassy ''CurrentOrderSummary
makeClassy ''CurrentOrderSummaryReport
makeClassy ''ExBestOffersOverrides
makeClassy ''ExchangePrices
makeClassy ''Event
makeClassy ''EventResult
makeClassy ''EventType
makeClassy ''EventTypeResult
makeClassy ''ItemDescription
makeClassy ''ListCompetitions
makeClassy ''ListCountries
makeClassy ''ListCurrentOrders
makeClassy ''ListClearedOrders
makeClassy ''ListEvents
makeClassy ''ListEventTypes
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
makeClassy ''RunnerId
makeClassy ''StartingPrices
makeClassy ''TimeRange

-- the number is how many characters to drop from a record field name to get
-- the JSON name in the API
deriveJSON (commonStruct 3) ''ClearedOrderSummary
deriveJSON (commonStruct 3) ''ClearedOrderSummaryReport
deriveJSON (commonStruct 2) ''Competition
deriveJSON (commonStruct 2) ''CompetitionResult
deriveJSON (commonStruct 3) ''CountryCodeResult
deriveJSON (commonStruct 2) ''CurrentOrderSummary
deriveJSON (commonStruct 2) ''CurrentOrderSummaryReport
deriveJSON (commonStruct 3) ''ExBestOffersOverrides
deriveJSON (commonStruct 3) ''ExchangePrices
deriveJSON (commonStruct 2) ''Event
deriveJSON (commonStruct 3) ''EventResult
deriveJSON (commonStruct 3) ''EventType
deriveJSON (commonStruct 4) ''EventTypeResult
deriveJSON (commonStruct 2) ''ItemDescription
deriveJSON (commonStruct 3) ''ListCompetitions
deriveJSON (commonStruct 4) ''ListCountries
deriveJSON (commonStruct 4) ''ListCurrentOrders
deriveJSON (commonStruct 4) ''ListClearedOrders
deriveJSON (commonStruct 3) ''ListEvents
deriveJSON (commonStruct 4) ''ListEventTypes
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
deriveJSON (commonStruct 3) ''RunnerId
deriveJSON (commonStruct 3) ''StartingPrices
deriveJSON (commonStruct 2) ''TimeRange

deriveJSON (commonEnum 0) ''BetStatus
deriveJSON (commonEnum 2) ''GroupBy
deriveJSON (commonEnum 0) ''MarketBettingType
deriveJSON (commonEnum 0) ''MarketProjection
deriveJSON (commonEnum 0) ''MarketSort
deriveJSON (commonEnum 0) ''MarketStatus
deriveJSON (commonEnum 0) ''MatchProjection
deriveJSON (commonEnum 0) ''OrderBy
deriveJSON (commonEnum 2) ''OrderProjection
deriveJSON (commonEnum 0) ''OrderStatus
deriveJSON (commonEnum 0) ''OrderType
deriveJSON (commonEnum 2) ''PersistenceType
deriveJSON (commonEnum 0) ''PriceData
deriveJSON (commonEnum 0) ''RollupModel
deriveJSON (commonEnum 0) ''RunnerStatus
deriveJSON (commonEnum 0) ''SortDir

deriveJSON (commonEnum 0) ''APIExceptionCode

instance Request ListCompetitions [CompetitionResult] where
    requestMethod _ = "listCompetitions"

instance Request ListCountries [CountryCodeResult] where
    requestMethod _ = "listCountries"

instance Request ListCurrentOrders CurrentOrderSummaryReport where
    requestMethod _ = "listCurrentOrders"

instance Request ListClearedOrders ClearedOrderSummaryReport where
    requestMethod _ = "listClearedOrders"

instance Request ListEvents [EventResult] where
    requestMethod _ = "listEvents"

instance Request ListEventTypes [EventTypeResult] where
    requestMethod _ = "listEventTypes"

instance Request ListMarketBook [MarketBook] where
    requestMethod _ = "listMarketBook"

instance Request ListMarketCatalogue [MarketCatalogue] where
    requestMethod _ = "listMarketCatalogue"

-- | Data types in Betfair API that have some kind of default.
--
-- All `Maybe` fields in records are set to `Nothing`, all lists and sets will
-- be empty. If there is no sensible default then the type won't implement this
-- typeclass.
class Default a where
    def :: a

instance Default ExBestOffersOverrides where
    def = ExBestOffersOverridesC Nothing Nothing Nothing Nothing Nothing

instance Default ExchangePrices where
    def = ExchangePricesC Nothing Nothing Nothing

instance Default ListCompetitions where
    def = ListCompetitionsC
          { _lcfilter = def
          , _lclocale = Nothing }

instance Default ListCountries where
    def = ListCountriesC
          { _lcsfilter = def
          , _lcslocale = Nothing }

instance Default ListCurrentOrders where
    def = ListCurrentOrdersC Nothing Nothing Nothing Nothing Nothing Nothing
                             Nothing Nothing

-- | `_lcrbetStatus` is `Settled`.
instance Default ListClearedOrders where
    def = ListClearedOrdersC Settled Nothing Nothing Nothing Nothing Nothing
                             Nothing Nothing Nothing Nothing Nothing Nothing
                             Nothing

instance Default ListEvents where
    def = ListEventsC
          { _lefilter = def
          , _lelocale = Nothing }

instance Default ListEventTypes where
    def = ListEventTypesC
          { _letfilter = def
          , _letlocale = Nothing }

instance Default ListMarketBook where
    def = ListMarketBookC
          { _lmbmarketIds = []
          , _lmbpriceProjection = Nothing
          , _lmborderProjection = Nothing
          , _lmbmatchProjection = Nothing
          , _lmbcurrencyCode = Nothing
          , _lmblocale = Nothing }

-- | `_lmcmaxResults` is set to 1000.
instance Default ListMarketCatalogue where
    def = ListMarketCatalogueC
          { _lmcfilter = def
          , _lmcmarketProjection = Nothing
          , _lmcsort = Nothing
          , _lmcmaxResults = 1000
          , _lmclocale = Nothing }

instance Default MarketFilter where
    def = MarketFilterC Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Default PriceProjection where
    def = PriceProjectionC Nothing Nothing Nothing Nothing


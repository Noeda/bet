{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | This module defines most data types in the betting API of Betfair API,
-- corresponding to version 2.0. Refer to Betfair API documentation for
-- meanings of the values.
--
-- <https://developer.betfair.com/default/api-s-and-services/sports-api/sports-overview/>
--
-- This module does not have a proper Haddock index so pay attention here.
--
-- Where possible, data types are in 1:1 correspondence to the ones documented
-- in the API. However, because there are name clashes, most record field names
-- are prefixed with an abbreviation of the data type they are defined in. All
-- recordful constructors have C at their end.
--
-- There are lenses for every record field, giving you overloaded fields. The
-- lens name will have the prefix stripped off and the first character
-- lowercased. For example, `cStatus` record field in `CurrentOrderSummary` has
-- a corresponding lens called `status`, implemented in `HasStatus` type class.
--
-- Records that can be used as requests implement `Request` type class. Records
-- that have some kind of sensible default implement `Default` type class.
--
-- `HasBet` is a helper typeclass for things that have a `Bet` inside them in
-- some way.
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

module Network.Betfair.Types where

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

data ActionPerformed
    = ANone
    | ACancellationRequestSubmitted
    | AAllBetsCancelled
    | ASomeBetsNotCancelled
    | ACancellationRequestError
    | ACancellationStatusUnknown
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data BetStatus
    = Settled
    | Voided
    | Lapsed
    | Cancelled
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data ExecutionReportErrorCode
    = RErrorInMatcher
    | RProcessedWithErrors
    | RBetActionError
    | RInvalidAccountState
    | RInvalidWalletStatus
    | RInsufficientFunds
    | RLossLimitExceeded
    | RMarketSuspended
    | RMarketNotOpenForBetting
    | RDuplicateTransaction
    | RInvalidOrder
    | RInvalidMarketId
    | RPermissionDenied
    | RDuplicateBetids
    | RNoActionRequired
    | RServiceUnavailable
    | RRejectedByRegulator
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data ExecutionReportStatus
    = ESuccess
    | EFailure
    | EProcessedWithErrors
    | ETimeout
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data GroupBy
    = GBEventType
    | GBEvent
    | GBMarket
    | GBSide
    | GBBet
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data InstructionReportErrorCode
    = InvalidBetSize
    | InvalidRunner
    | BetTakenOrLapsed
    | BetInProgress
    | RunnerRemoved
    | MarketNotOpenForBetting
    | LossLimitExceeded
    | MarketNotOpenForBspBetting
    | InvalidPriceEdit
    | InvalidOdds
    | InsufficientFunds
    | InvalidPersistenceType
    | ErrorInMatcher
    | InvalidBackLayCombination
    | ErrorInOrder
    | InvalidBidType
    | InvalidBetId
    | CancelledNotPlaced
    | RelatedActionFailed
    | NoActionRequired
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )

data InstructionReportStatus
    = Success
    | Failure
    | Timeout
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

data TimeGranularity
    = Days
    | Hours
    | Minutes
    deriving ( Eq, Ord, Show, Read, Typeable, Enum )


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

data CancelExecutionReport = CancelExecutionReportC
    { ceCustomerRef :: Maybe Text
    , ceStatus :: ExecutionReportStatus
    , ceErrorCode :: Maybe ExecutionReportErrorCode
    , ceMarketId :: Maybe MarketId
    , ceInstructionReports :: [CancelInstructionReport] }
    deriving ( Eq, Ord, Show, Read, Typeable )

data CancelInstruction = CancelInstructionC
    { ciBetId :: BetId
    , ciSizeReduction :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data CancelInstructionReport = CancelInstructionReportC
    { ciStatus :: InstructionReportStatus
    , ciInstructionReportErrorCode :: Maybe InstructionReportErrorCode
    , ciInstruction :: Maybe CancelInstruction
    , ciSizeCancelled :: Double
    , ciCancelledDate :: Maybe UTCTime }
    deriving ( Eq, Ord, Show, Read, Typeable )

data CancelOrders = CancelOrdersC
    { caMarketId :: Maybe MarketId
    , caInstructions :: Maybe [CancelInstruction]
    , caCustomerRef :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ClearedOrderSummary = ClearedOrderSummaryC
    { coEventTypeId :: EventTypeId
    , coEventId :: EventId
    , coMarketId :: MarketId
    , coSelectionId :: SelectionId
    , coHandicap :: Double
    , coBetId :: BetId
    , coPlacedDate :: UTCTime
    , coPersistenceType :: Maybe PersistenceType
    , coOrderType :: Maybe OrderType
    , coSide :: Maybe Side
    , coItemDescription :: Maybe ItemDescription
    , coPriceRequested :: Maybe Double
    , coSettledDate :: Maybe UTCTime
    , coBetCount :: Int
    , coCommission :: Maybe Double
    , coPriceMatched :: Maybe Double
    , coPriceReduced :: Maybe Bool
    , coSizeSettled :: Maybe Double
    , coProfit :: Double
    , coSizeCancelled :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Competition = CompetitionC
    { cId :: CompetitionId
    , cName :: Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data CompetitionResult = CompetitionResultC
    { cCompetition :: Competition
    , cMarketCount :: Int
    , cCompetitionRegion :: Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data CountryCodeResult = CountryCodeResultC
    { ccCountryCode :: Text
    , ccMarketCount :: Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ClearedOrderSummaryReport = ClearedOrderSummaryReportC
    { coClearedOrders :: [ClearedOrderSummary]
    , coMoreAvailable :: Bool }
    deriving ( Eq, Ord, Show, Read, Typeable )

data CurrentOrderSummary = CurrentOrderSummaryC
    { cBetId :: BetId
    , cMarketId :: MarketId
    , cSelectionId :: SelectionId
    , cHandicap :: Double
    , cPriceSize :: PriceSize
    , cBspLiability :: Double
    , cSide :: Side
    , cStatus :: OrderStatus
    , cPersistenceType :: PersistenceType
    , cOrderType :: OrderType
    , cPlacedDate :: UTCTime
    , cMatchedDate :: Maybe UTCTime
    , cAveragePriceMatched :: Maybe Double
    , cSizeMatched :: Maybe Double
    , cSizeRemaining :: Maybe Double
    , cSizeLapsed :: Maybe Double
    , cSizeCancelled :: Maybe Double
    , cSizeVoided :: Maybe Double
    , cRegulatorAuthCode :: Maybe Text
    , cRegulatorCode :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data CurrentOrderSummaryReport = CurrentOrderSummaryReportC
    { cCurrentOrders :: [CurrentOrderSummary]
    , cMoreAvailable :: Bool }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ExBestOffersOverrides = ExBestOffersOverridesC
    { ebbestPricesDepth :: Maybe Int
    , ebRollupModel :: Maybe RollupModel
    , ebRollupLimit :: Maybe Int
    , ebRollupLiabilityThreshold :: Maybe Double
    , ebRollupLiabilityFactor :: Maybe Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ExchangePrices = ExchangePricesC
    { epAvailableToBack :: Maybe [PriceSize]
    , epAvailableToLay :: Maybe [PriceSize]
    , epTradedVolume :: Maybe [PriceSize] }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Event = EventC
    { eId :: EventId
    , eName :: Text
    , eCountryCode :: Maybe Country
    , eTimezone :: Text
    , eVenue :: Maybe Text
    , eOpenDate :: UTCTime }
    deriving ( Eq, Ord, Show, Read, Typeable )

data EventResult = EventResultC
    { erEvent :: Event
    , erMarketCount :: Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data EventType = EventTypeC
    { etId :: EventTypeId
    , etName :: Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data EventTypeResult = EventTypeResultC
    { etrEventType :: EventType
    , etrMarketCount :: Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

newtype Heartbeat = HeartbeatC
        { hPreferredTimeoutSeconds :: Int }
        deriving ( Eq, Ord, Show, Read, Typeable )

data HeartbeatReport = HeartbeatReportC
    { hActionPerformed :: ActionPerformed
    , hActualTimeoutSeconds :: Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ItemDescription = ItemDescriptionC
    { iEventTypeDesc :: Maybe Text
    , iEventDesc :: Maybe Text
    , iMarketDesc :: Maybe Text
    , iMarketStartTime :: Maybe UTCTime
    , iRunnerDesc :: Maybe Text
    , iNumberOfWinners :: Maybe Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data LimitOnCloseOrder = LimitOnCloseOrderC
    { loLiability :: Double
    , loPrice :: Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data LimitOrder = LimitOrderC
    { lSize :: Double
    , lPrice :: Double
    , lPersistenceType :: PersistenceType }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListCompetitions = ListCompetitionsC
    { lcFilter :: MarketFilter
    , lcLocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListCountries = ListCountriesC
    { lcsFilter :: MarketFilter
    , lcsLocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListCurrentOrders = ListCurrentOrdersC
    { lcoBetIds :: Maybe (S.Set BetId)
    , lcoMarketIds :: Maybe (S.Set MarketId)
    , lcoOrderProjection :: Maybe OrderProjection
    , lcoDateRange :: Maybe TimeRange
    , lcoOrderBy :: Maybe OrderBy
    , lcoSortDir :: Maybe SortDir
    , lcoFromRecord :: Maybe Int
    , lcoRecordCount :: Maybe Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListClearedOrders = ListClearedOrdersC
    { lcrBetStatus :: BetStatus
    , lcrEventTypeIds :: Maybe (S.Set EventTypeId)
    , lcrEventIds :: Maybe (S.Set EventId)
    , lcrMarketIds :: Maybe (S.Set MarketId)
    , lcrRunnerIds :: Maybe (S.Set RunnerId)
    , lcrBetIds :: Maybe (S.Set BetId)
    , lcrSide :: Maybe Side
    , lcrSettledDateRange :: Maybe TimeRange
    , lcrGroupBy :: Maybe GroupBy
    , lcrIncludeItemDescription :: Maybe Bool
    , lcrLocale :: Maybe Text
    , lcrFromRecord :: Maybe Int
    , lcrRecordCount :: Maybe Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListEvents = ListEventsC
    { leFilter :: MarketFilter
    , leLocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListEventTypes = ListEventTypesC
    { letFilter :: MarketFilter
    , letLocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListMarketBook = ListMarketBookC
    { lmbMarketIds :: [MarketId]
    , lmbPriceProjection :: Maybe PriceProjection
    , lmbOrderProjection :: Maybe OrderProjection
    , lmbMatchProjection :: Maybe MatchProjection
    , lmbCurrencyCode :: Maybe Text
    , lmbLocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListMarketCatalogue = ListMarketCatalogueC
    { lmcFilter :: MarketFilter
    , lmcMarketProjection :: Maybe (S.Set MarketProjection)
    , lmcSort :: Maybe MarketSort
    , lmcMaxResults :: Int
    , lmcLocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListMarketProfitAndLoss = ListMarketProfitAndLossC
    { lmMarketIds :: S.Set MarketId
    , lmIncludeSettledBets :: Maybe Bool
    , lmIncludeBspBets :: Maybe Bool
    , lmNetOfCommission :: Maybe Bool }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListMarketTypes = ListMarketTypesC
    { lmtFilter :: MarketFilter
    , lmtLocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListTimeRanges = ListTimeRangesC
    { ltFilter :: MarketFilter
    , ltGranularity :: TimeGranularity }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ListVenues = ListVenuesC
    { lvFilter :: MarketFilter
    , lvLocale :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketBook = MarketBookC
    { mbMarketId :: MarketId
    , mbIsMarketDataDelayed :: Bool
    , mbStatus :: Maybe MarketStatus
    , mbBetDelay :: Maybe Int
    , mbBspReconciled :: Maybe Bool
    , mbComplete :: Maybe Bool
    , mbInplay :: Maybe Bool
    , mbNumberOfWinners :: Maybe Int
    , mbNumberOfRunners :: Maybe Int
    , mbNumberOfActiveRunners :: Maybe Int
    , mbLastMatchTime :: Maybe UTCTime
    , mbTotalMatched :: Maybe Double
    , mbTotalAvailable :: Maybe Double
    , mbCrossMatching :: Maybe Bool
    , mbRunnersVoidable :: Maybe Bool
    , mbVersion :: Maybe Int
    , mbRunners :: [Runner] }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketCatalogue = MarketCatalogueC
    { mcMarketId :: MarketId
    , mcMarketName :: Text
    , mcMarketStartTime :: Maybe UTCTime
    , mcDescription :: Maybe MarketDescription
    , mcTotalMatched :: Maybe Double
    , mcRunners :: Maybe [RunnerCatalog]
    , mcEventType :: Maybe EventType
    , mcCompetition :: Maybe Competition
    , mcEvent :: Maybe Event }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketDescription = MarketDescriptionC
    { mdPersistenceEnabled :: Bool
    , mdBspMarket :: Bool
    , mdMarketTime :: UTCTime
    , mdSuspendTime :: UTCTime
    , mdSettleTime :: Maybe UTCTime
    , mdBettingType :: MarketBettingType
    , mdTurnInPlayEnabled :: Bool
    , mdMarketType :: Text
    , mdRegulator :: Text
    , mdMarketBaseRate :: Double
    , mdDiscountAllowed :: Bool
    , mdWallet :: Maybe Text
    , mdRules :: Maybe Text
    , mdRulesHasDate :: Maybe Bool
    , mdClarifications :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketFilter = MarketFilterC
    { mfTextQuery :: Maybe String
    , mfEventTypeIds :: Maybe (S.Set EventTypeId)
    , mfEventIds :: Maybe (S.Set EventId)
    , mfCompetitionIds :: Maybe (S.Set CompetitionId)
    , mfMarketIds :: Maybe (S.Set MarketId)
    , mfVenues :: Maybe (S.Set Venue)
    , mfBspOnly :: Maybe Bool
    , mfTurnInPlayEnabled :: Maybe Bool
    , mfInPlayOnly :: Maybe Bool
    , mfMarketBettingTypes :: Maybe (S.Set MarketBettingType)
    , mfMarketCountries :: Maybe (S.Set Country)
    , mfMarketTypeCodes :: Maybe (S.Set MarketTypeCode)
    , mfMarketStartTime :: Maybe TimeRange
    , mfWithOrders :: Maybe (S.Set OrderStatus) }
    deriving ( Eq, Ord, Show, Read, Typeable )

newtype MarketOnCloseOrder = MarketOnCloseOrderC
        { mLiability :: Double }
        deriving ( Eq, Ord, Show, Read, Typeable )

data MarketProfitAndLoss = MarketProfitAndLossC
    { mpMarketId :: Maybe MarketId
    , mpCommissionApplied :: Maybe Double
    , mpProfitAndLosses :: Maybe [RunnerProfitAndLoss] }
    deriving ( Eq, Ord, Show, Read, Typeable )

data MarketTypeResult = MarketTypeResultC
    { mtMarketType :: Text
    , mtMarketCount :: Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Match = MatchC
    { mBetId :: Maybe BetId
    , mMatchId :: Maybe MatchId
    , mSide :: Side
    , mPrice :: Double
    , mSize :: Double
    , mMatchDate :: Maybe UTCTime }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Order = OrderC
    { oBetId :: BetId
    , oOrderType :: OrderType
    , oStatus :: OrderStatus
    , oPersistenceType :: Maybe PersistenceType
    , oSide :: Side
    , oPrice :: Double
    , oSize :: Double
    , oBspLiability :: Double
    , oPlacedDate :: UTCTime
    , oAvgPriceMatched :: Maybe Double
    , oSizeMatched :: Maybe Double
    , oSizeRemaining :: Maybe Double
    , oSizeLapsed :: Maybe Double
    , oSizeCancelled :: Maybe Double
    , oSizeVoided :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data PlaceExecutionReport = PlaceExecutionReportC
    { peCustomerRef :: Maybe Text
    , peStatus :: ExecutionReportStatus
    , peErrorCode :: Maybe ExecutionReportErrorCode
    , peMarketId :: Maybe MarketId
    , peInstructionReports :: Maybe [PlaceInstructionReport] }
    deriving ( Eq, Ord, Show, Read, Typeable )

data PlaceInstruction = PlaceInstructionC
    { pOrderType :: OrderType
    , pSelectionId :: SelectionId
    , pHandicap :: Maybe Double
    , pSide :: Side
    , pLimitOrder :: Maybe LimitOrder
    , pLimitOnCloseOrder :: Maybe LimitOnCloseOrder
    , pMarketOnCloseOrder :: Maybe MarketOnCloseOrder }
    deriving ( Eq, Ord, Show, Read, Typeable )

data PlaceInstructionReport = PlaceInstructionReportC
    { piStatus :: InstructionReportStatus
    , piErrorCode :: Maybe InstructionReportErrorCode
    , piInstruction :: PlaceInstruction
    , piBetId :: Maybe Text
    , piPlacedDate :: Maybe UTCTime
    , piAveragePriceMatched :: Maybe Double
    , piSizeMatched :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data PlaceOrders = PlaceOrdersC
    { pMarketId :: MarketId
    , pInstructions :: [PlaceInstruction]
    , pCustomerRef :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data PriceProjection = PriceProjectionC
    { ppPriceData :: Maybe (S.Set PriceData)
    , ppExBestOffersOverrides :: Maybe ExBestOffersOverrides
    , ppVirtualize :: Maybe Bool
    , ppRolloverStakes :: Maybe Bool }
    deriving ( Eq, Ord, Show, Read, Typeable )

data PriceSize = PriceSizeC
    { psPrice :: Double
    , psSize :: Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ReplaceExecutionReport = ReplaceExecutionReportC
    { reCustomerRef :: Maybe Text
    , reStatus :: ExecutionReportStatus
    , reErrorCode :: Maybe ExecutionReportErrorCode
    , reMarketId :: Maybe Text
    , reInstructionReports :: Maybe [ReplaceInstructionReport] }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ReplaceInstruction = ReplaceInstructionC
    { rBetId :: BetId
    , rNewPrice :: Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ReplaceInstructionReport = ReplaceInstructionReportC
    { riStatus :: InstructionReportStatus
    , riErrorCode :: Maybe InstructionReportErrorCode
    , riCancelInstructionReport :: Maybe CancelInstructionReport
    , riPlaceInstructionReport :: Maybe PlaceInstructionReport }
    deriving ( Eq, Ord, Show, Read, Typeable )

data ReplaceOrders = ReplaceOrdersC
    { rMarketId :: MarketId
    , rInstructions :: [ReplaceInstruction]
    , rCustomerRef :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data Runner = RunnerC
    { rSelectionId :: SelectionId
    , rHandicap :: Double
    , rStatus :: RunnerStatus
    , rAdjustmentFactor :: Maybe Double -- the official documentation says
                                        -- this is required but it's lying.
                                        -- lying! so we use Maybe
    , rLastPriceTraded :: Maybe Double
    , rTotalMatched :: Maybe Double
    , rRemovalData :: Maybe UTCTime
    , rSp :: Maybe StartingPrices
    , rEx :: Maybe ExchangePrices
    , rOrders :: Maybe [Order]
    , rMatches :: Maybe [Match] }
    deriving ( Eq, Ord, Show, Read, Typeable )

data RunnerCatalog = RunnerCatalogC
    { rcSelectionId :: SelectionId
    , rcRunnerName :: Text
    , rcHandicap :: Double
    , rcSortPriority :: Int
    , rcMetadata :: M.Map Text Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data RunnerId = RunnerIdC
    { riMarketId :: MarketId
    , riSelectionId :: SelectionId
    , riHandicap :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data RunnerProfitAndLoss = RunnerProfitAndLossC
    { rpSelectionId :: Maybe SelectionId
    , rpIfWin :: Maybe Double
    , rpIfLose :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data StartingPrices = StartingPricesC
    { spNearPrice :: Maybe Double
    , spFarPrice :: Maybe Double
    , spBackStakeTaken :: Maybe [PriceSize]
    , spLayLiabilityTaken :: Maybe [PriceSize]
    , spActualSP :: Maybe Double }
    deriving ( Eq, Ord, Show, Read, Typeable )

data TimeRange = TimeRangeC
    { tFrom :: UTCTime
    , tTo :: UTCTime }
    deriving ( Eq, Ord, Show, Read, Typeable )

data TimeRangeResult = TimeRangeResultC
    { tTimeRange :: TimeRange
    , tMarketCount :: Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

data UpdateExecutionReport = UpdateExecutionReportC
    { ueCustomerRef :: Maybe Text
    , ueStatus :: ExecutionReportStatus
    , ueErrorCode :: Maybe ExecutionReportErrorCode
    , ueMarketId :: Maybe Text
    , ueInstructionReports :: Maybe [UpdateInstructionReport] }
    deriving ( Eq, Ord, Show, Read, Typeable )

data UpdateInstruction = UpdateInstructionC
    { uBetId :: BetId
    , uNewPersistenceType :: PersistenceType }
    deriving ( Eq, Ord, Show, Read, Typeable )

data UpdateInstructionReport = UpdateInstructionReportC
    { uiStatus :: InstructionReportStatus
    , uiErrorCode :: Maybe InstructionReportErrorCode
    , uiInstruction :: UpdateInstruction }
    deriving ( Eq, Ord, Show, Read, Typeable )

data UpdateOrders = UpdateOrdersC
    { uMarketId :: MarketId
    , uInstructions :: [UpdateInstruction]
    , uCustomerRef :: Maybe Text }
    deriving ( Eq, Ord, Show, Read, Typeable )

data VenueResult = VenueResultC
    { vVenue :: Text
    , vMarketCount :: Int }
    deriving ( Eq, Ord, Show, Read, Typeable )

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

makeLensesWith abbreviatedFields ''CancelExecutionReport
makeLensesWith abbreviatedFields ''CancelInstruction
makeLensesWith abbreviatedFields ''CancelInstructionReport
makeLensesWith abbreviatedFields ''CancelOrders
makeLensesWith abbreviatedFields ''ClearedOrderSummary
makeLensesWith abbreviatedFields ''ClearedOrderSummaryReport
makeLensesWith abbreviatedFields ''Competition
makeLensesWith abbreviatedFields ''CompetitionResult
makeLensesWith abbreviatedFields ''CountryCodeResult
makeLensesWith abbreviatedFields ''CurrentOrderSummary
makeLensesWith abbreviatedFields ''CurrentOrderSummaryReport
makeLensesWith abbreviatedFields ''ExBestOffersOverrides
makeLensesWith abbreviatedFields ''ExchangePrices
makeLensesWith abbreviatedFields ''Event
makeLensesWith abbreviatedFields ''EventResult
makeLensesWith abbreviatedFields ''EventType
makeLensesWith abbreviatedFields ''EventTypeResult
makeLensesWith abbreviatedFields ''ItemDescription
makeLensesWith abbreviatedFields ''LimitOnCloseOrder
makeLensesWith abbreviatedFields ''LimitOrder
makeLensesWith abbreviatedFields ''ListCompetitions
makeLensesWith abbreviatedFields ''ListCountries
makeLensesWith abbreviatedFields ''ListCurrentOrders
makeLensesWith abbreviatedFields ''ListClearedOrders
makeLensesWith abbreviatedFields ''ListEvents
makeLensesWith abbreviatedFields ''ListEventTypes
makeLensesWith abbreviatedFields ''ListMarketBook
makeLensesWith abbreviatedFields ''ListMarketCatalogue
makeLensesWith abbreviatedFields ''ListMarketProfitAndLoss
makeLensesWith abbreviatedFields ''ListMarketTypes
makeLensesWith abbreviatedFields ''ListTimeRanges
makeLensesWith abbreviatedFields ''ListVenues
makeLensesWith abbreviatedFields ''MarketBook
makeLensesWith abbreviatedFields ''MarketCatalogue
makeLensesWith abbreviatedFields ''MarketDescription
makeLensesWith abbreviatedFields ''MarketFilter
makeLensesWith abbreviatedFields ''MarketOnCloseOrder
makeLensesWith abbreviatedFields ''MarketProfitAndLoss
makeLensesWith abbreviatedFields ''MarketTypeResult
makeLensesWith abbreviatedFields ''Match
makeLensesWith abbreviatedFields ''Order
makeLensesWith abbreviatedFields ''PlaceExecutionReport
makeLensesWith abbreviatedFields ''PlaceInstruction
makeLensesWith abbreviatedFields ''PlaceInstructionReport
makeLensesWith abbreviatedFields ''PlaceOrders
makeLensesWith abbreviatedFields ''PriceProjection
makeLensesWith abbreviatedFields ''PriceSize
makeLensesWith abbreviatedFields ''ReplaceExecutionReport
makeLensesWith abbreviatedFields ''ReplaceInstruction
makeLensesWith abbreviatedFields ''ReplaceInstructionReport
makeLensesWith abbreviatedFields ''ReplaceOrders
makeLensesWith abbreviatedFields ''Runner
makeLensesWith abbreviatedFields ''RunnerCatalog
makeLensesWith abbreviatedFields ''RunnerId
makeLensesWith abbreviatedFields ''RunnerProfitAndLoss
makeLensesWith abbreviatedFields ''StartingPrices
makeLensesWith abbreviatedFields ''TimeRange
makeLensesWith abbreviatedFields ''TimeRangeResult
makeLensesWith abbreviatedFields ''UpdateExecutionReport
makeLensesWith abbreviatedFields ''UpdateInstruction
makeLensesWith abbreviatedFields ''UpdateInstructionReport
makeLensesWith abbreviatedFields ''UpdateOrders
makeLensesWith abbreviatedFields ''VenueResult

-- the number is how many characters to drop from a record field name to get
-- the JSON name in the API
deriveJSON commonStruct ''CancelExecutionReport
deriveJSON commonStruct ''CancelInstruction
deriveJSON commonStruct ''CancelInstructionReport
deriveJSON commonStruct ''CancelOrders
deriveJSON commonStruct ''ClearedOrderSummary
deriveJSON commonStruct ''ClearedOrderSummaryReport
deriveJSON commonStruct ''Competition
deriveJSON commonStruct ''CompetitionResult
deriveJSON commonStruct ''CountryCodeResult
deriveJSON commonStruct ''CurrentOrderSummary
deriveJSON commonStruct ''CurrentOrderSummaryReport
deriveJSON commonStruct ''ExBestOffersOverrides
deriveJSON commonStruct ''ExchangePrices
deriveJSON commonStruct ''Event
deriveJSON commonStruct ''EventResult
deriveJSON commonStruct ''EventType
deriveJSON commonStruct ''EventTypeResult
deriveJSON commonStruct ''Heartbeat
deriveJSON commonStruct ''HeartbeatReport
deriveJSON commonStruct ''ItemDescription
deriveJSON commonStruct ''LimitOnCloseOrder
deriveJSON commonStruct ''LimitOrder
deriveJSON commonStruct ''ListCompetitions
deriveJSON commonStruct ''ListCountries
deriveJSON commonStruct ''ListCurrentOrders
deriveJSON commonStruct ''ListClearedOrders
deriveJSON commonStruct ''ListEvents
deriveJSON commonStruct ''ListEventTypes
deriveJSON commonStruct ''ListMarketBook
deriveJSON commonStruct ''ListMarketCatalogue
deriveJSON commonStruct ''ListMarketProfitAndLoss
deriveJSON commonStruct ''ListMarketTypes
deriveJSON commonStruct ''ListTimeRanges
deriveJSON commonStruct ''ListVenues
deriveJSON commonStruct ''MarketBook
deriveJSON commonStruct ''MarketCatalogue
deriveJSON commonStruct ''MarketDescription
deriveJSON commonStruct ''MarketFilter
deriveJSON commonStruct ''MarketOnCloseOrder
deriveJSON commonStruct ''MarketProfitAndLoss
deriveJSON commonStruct ''MarketTypeResult
deriveJSON commonStruct ''Match
deriveJSON commonStruct ''Order
deriveJSON commonStruct ''PlaceExecutionReport
deriveJSON commonStruct ''PlaceInstruction
deriveJSON commonStruct ''PlaceInstructionReport
deriveJSON commonStruct ''PlaceOrders
deriveJSON commonStruct ''PriceProjection
deriveJSON commonStruct ''PriceSize
deriveJSON commonStruct ''ReplaceExecutionReport
deriveJSON commonStruct ''ReplaceInstruction
deriveJSON commonStruct ''ReplaceInstructionReport
deriveJSON commonStruct ''ReplaceOrders
deriveJSON commonStruct ''Runner
deriveJSON commonStruct ''RunnerCatalog
deriveJSON commonStruct ''RunnerId
deriveJSON commonStruct ''RunnerProfitAndLoss
deriveJSON commonStruct ''StartingPrices
deriveJSON commonStruct ''TimeRange
deriveJSON commonStruct ''TimeRangeResult
deriveJSON commonStruct ''UpdateExecutionReport
deriveJSON commonStruct ''UpdateInstruction
deriveJSON commonStruct ''UpdateInstructionReport
deriveJSON commonStruct ''UpdateOrders
deriveJSON commonStruct ''VenueResult

deriveJSON (commonEnum 1) ''ActionPerformed
deriveJSON (commonEnum 0) ''BetStatus
deriveJSON (commonEnum 1) ''ExecutionReportErrorCode
deriveJSON (commonEnum 1) ''ExecutionReportStatus
deriveJSON (commonEnum 2) ''GroupBy
deriveJSON (commonEnum 0) ''InstructionReportErrorCode
deriveJSON (commonEnum 0) ''InstructionReportStatus
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
deriveJSON (commonEnum 0) ''TimeGranularity

deriveJSON (commonEnum 0) ''APIExceptionCode

instance Request CancelOrders CancelExecutionReport where
    requestMethod _ = "SportsAPING/v1.0/cancelOrders"
    requestUrl _ = bettingUrl

instance Request ListCompetitions [CompetitionResult] where
    requestMethod _ = "SportsAPING/v1.0/listCompetitions"
    requestUrl _ = bettingUrl

instance Request ListCountries [CountryCodeResult] where
    requestMethod _ = "SportsAPING/v1.0/listCountries"
    requestUrl _ = bettingUrl

instance Request ListCurrentOrders CurrentOrderSummaryReport where
    requestMethod _ = "SportsAPING/v1.0/listCurrentOrders"
    requestUrl _ = bettingUrl

instance Request ListClearedOrders ClearedOrderSummaryReport where
    requestMethod _ = "SportsAPING/v1.0/listClearedOrders"
    requestUrl _ = bettingUrl

instance Request ListEvents [EventResult] where
    requestMethod _ = "SportsAPING/v1.0/listEvents"
    requestUrl _ = bettingUrl

instance Request ListEventTypes [EventTypeResult] where
    requestMethod _ = "SportsAPING/v1.0/listEventTypes"
    requestUrl _ = bettingUrl

instance Request ListMarketBook [MarketBook] where
    requestMethod _ = "SportsAPING/v1.0/listMarketBook"
    requestUrl _ = bettingUrl

instance Request ListMarketCatalogue [MarketCatalogue] where
    requestMethod _ = "SportsAPING/v1.0/listMarketCatalogue"
    requestUrl _ = bettingUrl

instance Request ListMarketProfitAndLoss [MarketProfitAndLoss] where
    requestMethod _ = "SportsAPING/v1.0/listMarketProfitAndLoss"
    requestUrl _ = bettingUrl

instance Request ListMarketTypes [MarketTypeResult] where
    requestMethod _ = "SportsAPING/v1.0/listMarketTypes"
    requestUrl _ = bettingUrl

instance Request ListTimeRanges [TimeRangeResult] where
    requestMethod _ = "SportsAPING/v1.0/listTimeRanges"
    requestUrl _ = bettingUrl

instance Request ListVenues [VenueResult] where
    requestMethod _ = "SportsAPING/v1.0/listVenues"
    requestUrl _ = bettingUrl

instance Request PlaceOrders PlaceExecutionReport where
    requestMethod _ = "SportsAPING/v1.0/placeOrders"
    requestUrl _ = bettingUrl

instance Request ReplaceOrders ReplaceExecutionReport where
    requestMethod _ = "SportsAPING/v1.0/replaceOrders"
    requestUrl _ = bettingUrl

instance Request UpdateOrders UpdateExecutionReport where
    requestMethod _ = "SportsAPING/v1.0/updateOrders"
    requestUrl _ = bettingUrl

instance Request Heartbeat HeartbeatReport where
    requestMethod _ = "HeartbeatAPING/v1.0/heartbeat"
    requestUrl _ = heartbeatUrl

-- | Data types in Betfair API that have some kind of default.
--
-- All `Maybe` fields in records are set to `Nothing`, all lists and sets will
-- be empty. If there is no sensible default then the type won't implement this
-- typeclass.
class Default a where
    def :: a

instance Default CancelOrders where
    def = CancelOrdersC Nothing Nothing Nothing

instance Default ExBestOffersOverrides where
    def = ExBestOffersOverridesC Nothing Nothing Nothing Nothing Nothing

instance Default ExchangePrices where
    def = ExchangePricesC Nothing Nothing Nothing

instance Default ListCompetitions where
    def = ListCompetitionsC
          { lcFilter = def
          , lcLocale = Nothing }

instance Default ListCountries where
    def = ListCountriesC
          { lcsFilter = def
          , lcsLocale = Nothing }

instance Default ListCurrentOrders where
    def = ListCurrentOrdersC Nothing Nothing Nothing Nothing Nothing Nothing
                             Nothing Nothing

-- | `lcrBetStatus` is `Settled`.
instance Default ListClearedOrders where
    def = ListClearedOrdersC Settled Nothing Nothing Nothing Nothing Nothing
                             Nothing Nothing Nothing Nothing Nothing Nothing
                             Nothing

instance Default ListEvents where
    def = ListEventsC
          { leFilter = def
          , leLocale = Nothing }

instance Default ListEventTypes where
    def = ListEventTypesC
          { letFilter = def
          , letLocale = Nothing }

instance Default ListMarketBook where
    def = ListMarketBookC
          { lmbMarketIds = []
          , lmbPriceProjection = Nothing
          , lmbOrderProjection = Nothing
          , lmbMatchProjection = Nothing
          , lmbCurrencyCode = Nothing
          , lmbLocale = Nothing }

-- | `lmcMaxResults` is set to 1000.
instance Default ListMarketCatalogue where
    def = ListMarketCatalogueC
          { lmcFilter = def
          , lmcMarketProjection = Nothing
          , lmcSort = Nothing
          , lmcMaxResults = 1000
          , lmcLocale = Nothing }

instance Default ListMarketProfitAndLoss where
    def = ListMarketProfitAndLossC S.empty Nothing Nothing Nothing

instance Default ListMarketTypes where
    def = ListMarketTypesC
          { lmtFilter = def
          , lmtLocale = Nothing }

-- | `TimeGranularity` is set to `Days`.
instance Default ListTimeRanges where
    def = ListTimeRangesC
          { ltFilter = def
          , ltGranularity = Days }

instance Default ListVenues where
    def = ListVenuesC
          { lvFilter = def
          , lvLocale = Nothing }

instance Default MarketFilter where
    def = MarketFilterC Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Default PriceProjection where
    def = PriceProjectionC Nothing Nothing Nothing Nothing


-- | Things that have a non-ambiguous way to extract a `Bet` from them.
--
-- Note that terminology does not match up correctly, `Bet` is about
-- odds-markets but some bets don't work with odds. However with this
-- construction, price is matched to odds and size is matched to stake.
class HasBet a where
    bet :: Lens' a (Bet Double Double)

instance HasBet CurrentOrderSummary where
    bet = lens (\cosum -> Bet (cSide cosum)
                            (psPrice $ cPriceSize cosum)
                            (psSize $ cPriceSize cosum))
               (\old new -> old { cSide = new^.betType
                                , cPriceSize = PriceSizeC
                                    { psPrice = new^.odds
                                    , psSize = new^.stake } })

instance HasBet Match where
    bet = lens (\m -> Bet (mSide m) (mPrice m) (mSize m))
               (\old new -> old { mSide = new^.betType
                                , mPrice = new^.odds
                                , mSize = new^.stake })

instance HasBet Order where
    bet = lens (\o -> Bet (oSide o) (oPrice o) (oSize o))
               (\old new -> old { oSide = new^.betType
                                , oPrice = new^.odds
                                , oSize = new^.stake })



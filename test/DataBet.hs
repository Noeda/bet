{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DataBet
    ( dataBetTestGroup )
    where

import Control.Applicative
import Control.Lens
import Data.Bet
import Data.Semigroup
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

instance (Arbitrary odds, Arbitrary money) => Arbitrary (Bet odds money) where
    arbitrary = Bet <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BetType where
    arbitrary = do
        x <- arbitrary
        return $ if x then Back else Lay

asBetD :: Bet Double Double -> Bet Double Double
asBetD = id

dataBetTestGroup = $(testGroupGenerator)

prop_bettype_pattern (asBetD -> bet) =
    case bet of
        BetType Back -> bet^.betType == Back
        BetType Lay -> bet^.betType == Lay
        _ -> error "impossible"

prop_bet_semigroup (asBetD -> bet) (asBetD -> bet2) =
    let mbet = bimap Sum Sum bet
        mbet2 = bimap Sum Sum bet2
        bet3 = bimap getSum getSum $ mbet <> mbet2
     in bet3^.stake == bet^.stake + bet2^.stake

prop_betflipped_semigroup (asBetD -> bet) (asBetD -> bet2) =
    let mbet = bimap Sum Sum $ BetFlipped bet
        mbet2 = bimap Sum Sum $ BetFlipped $ bet2
        bet3 = getFlipped $ bimap getSum getSum $ mbet <> mbet2
     in bet3^.odds == bet^.odds + bet2^.odds

prop_back_liability (asBetD -> bet) = bet^.betType == Back ==>
    bet^.liability == bet^.stake

prop_back_set_liability (asBetD -> bet) z = bet^.betType == Back ==>
    (bet & liability .~ z)^.stake == z

prop_lay_liability (asBetD -> bet) = bet^.betType == Lay ==>
    bet^.liability == bet^.stake * (bet^.odds-1)

prop_lay_set_liability (asBetD -> bet) = bet^.betType == Lay ==>
    abs ((bet & liability .~ (bet^.liability))^.stake - bet^.stake) < 0.001


{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

-- | Data types and functions to work with odds and stakes.
--

module Data.Bet
    (
    -- * Bets
      BetType(..)
    , oppositeBetType
    , betBool
    , Bet(..)
    , BetFlipped(..)
    , betType
    , odds
    , stake
    -- ** Derived lenses
    -- | The values these lenses manipulate are calculated on the fly and not
    -- stored directly. These may not necessarily follow lens laws with 100%
    -- accuracy but only because there are inaccuracies in floating point
    -- numerical values.
    , liability
    , winningPotential
    -- ** Pattern synonyms
    , pattern BetType
    , pattern BetWithLiability
    -- * Choosing stakes
    , bestTradingStake
    , bestTradingStake2
    )
    where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Bifoldable
import Data.Traversable
import Data.Typeable

-- | A bet type. In a betting exchange, you usually can choose if you want to
-- make a back or lay bet. With traditional bookmakers, you usually only back.
data BetType = Back | Lay
               deriving ( Eq, Ord, Show, Read, Typeable, Enum )

-- | An `Iso` from `BetType` to `Bool`.
betBool :: Iso' BetType Bool
betBool = iso (\case
                  Back -> False
                  Lay  -> True)
              (\case
                  False -> Back
                  True  -> Lay)
{-# INLINEABLE betBool #-}

-- | Returns the opposite bet type.
oppositeBetType :: BetType -> BetType
oppositeBetType Back = Lay
oppositeBetType Lay = Back

-- | Describes a bet in terms of its (European) odds and the stake size.
--
-- @
--     Bet odds money = Bet BetType odds money
--          ^     ^
--          |     |
--          |     +--- Data type of the money to use.
--          |
--          +-- Describes the data type used as odds.
--              Some betting environments don't allow just any odds so
--              it may be useful to use this type variable to restrict
--              available odds.
-- @
--
data Bet odds money = Bet !BetType odds money
                      deriving ( Traversable
                               , Foldable
                               , Typeable
                               , Functor
                               , Read
                               , Show
                               , Ord
                               , Eq )

instance Foldable1 (Bet odds)
instance Bifoldable1 Bet

instance Bifoldable Bet where
    bifoldMap f g (Bet _ o m) = f o `mappend` g m
    {-# INLINEABLE bifoldMap #-}

instance Bitraversable Bet where
    bitraverse f g (Bet t o m) = Bet t <$> f o <*> g m

instance (Semigroup odds, Semigroup money) => Semigroup (Bet odds money) where
    b1 <> b2 = b1 & (stake %~ (<> b2^.stake)) .
                    (odds %~ (<> b2^.odds))

instance Bifunctor Bet where
    bimap fun1 fun2 = (odds %~ fun1) . (stake %~ fun2)
    {-# INLINEABLE bimap #-}

-- | A wrapper to use odds as last type argument.
newtype BetFlipped money odds = BetFlipped { getFlipped :: Bet odds money }
                                deriving ( Typeable
                                         , Read
                                         , Show
                                         , Ord
                                         , Eq )

instance Functor (BetFlipped money) where
    fmap f (getFlipped -> Bet t o m) = BetFlipped $ Bet t (f o) m

instance Foldable (BetFlipped money) where
    foldMap f (getFlipped -> Bet _ o _) = f o
    foldr f r (getFlipped -> Bet _ o _) = f o r

instance Traversable (BetFlipped money) where
    traverse f (getFlipped -> bet) =
        BetFlipped . (\x -> bet & odds .~ x) <$> f (bet^.odds)
    sequenceA (getFlipped -> Bet t o m) =
        BetFlipped <$> (Bet t <$> o <*> pure m)

instance (Semigroup odds, Semigroup money)
       => Semigroup (BetFlipped money odds) where
    (getFlipped -> b1) <> (getFlipped -> b2) =
        BetFlipped $ b1 <> b2

instance Bifunctor BetFlipped where
    bimap fun2 fun1 (getFlipped -> b) =
        BetFlipped $ b & (odds %~ fun1) . (stake %~ fun2)
    {-# INLINEABLE bimap #-}

instance Bifoldable BetFlipped where
    bifoldMap f g (getFlipped -> Bet _ o m) = g o `mappend` f m

instance Bifoldable1 BetFlipped

instance Bitraversable BetFlipped where
    bitraverse f g (getFlipped -> Bet t o m) =
        BetFlipped <$> (Bet t <$> g o <*> f m)

instance Foldable1 (BetFlipped money)

odds :: Lens (Bet odds1 money) (Bet odds2 money) odds1 odds2
odds = lens (\(Bet _ o _) -> o)
            (\(Bet t _ m) o -> Bet t o m)
{-# INLINEABLE odds #-}

stake :: Lens (Bet odds money1) (Bet odds money2) money1 money2
stake = lens (\(Bet _ _ m) -> m)
             (\(Bet t o _) m -> Bet t o m)
{-# INLINEABLE stake #-}

betType :: Lens' (Bet odds money) BetType
betType = lens (\(Bet t _ _) -> t)
               (\(Bet _ o m) t -> Bet t o m)
{-# INLINEABLE betType #-}

-- | Liability is the amount of money you stand to lose for a bet if you lose
-- it.
--
-- For back bets, liability equals stake. For lay bets, liability is stake
-- multiplied by (odds-1).
--
-- An unfortunate flaw of this function: odds and money data types need to be
-- the same.
liability :: (Fractional odds, odds ~ money) => Lens' (Bet odds money) money
liability = lens getLiability setLiability
{-# INLINEABLE liability #-}

setLiability :: (Fractional odds, odds ~ money)
             => Bet odds money -> money -> Bet odds money
setLiability bet@(BetType Back) new_m = bet & stake .~ new_m
setLiability bet@(Bet Lay o _) new_l = bet & stake .~ new_l/(o-1)
setLiability _ _ = error "setLiability: impossible"

getLiability :: (Num odds, odds ~ money) => Bet odds money -> money
getLiability (Bet Back _ m) = m
getLiability (Bet Lay o m) = m*(o-1)

-- | Winning potential tells you how much you could win if this bet pays.
--
-- This is profit value. If you back bet 5 dollars at odds 2.0 then your
-- winning potential is 5 dollars. (You will have 10 dollars if you started
-- with 5 dollars).
winningPotential :: (Fractional odds, odds ~ money)
                 => Lens' (Bet odds money) money
winningPotential = lens getWinningPotential setWinningPotential

getWinningPotential :: (Num odds, odds ~ money) => Bet odds money -> money
getWinningPotential (Bet Back o m) = (o-1) * m
getWinningPotential (Bet Lay _ m) = m

setWinningPotential :: (Fractional odds, odds ~ money)
                    => Bet odds money -> money -> Bet odds money
setWinningPotential bet@(BetType Back) new_w =
    bet & stake .~ new_w / (bet^.odds - 1)
setWinningPotential bet new_w =
    bet & stake .~ new_w

-- | Match the bet type only.
pattern BetType b <- Bet b _ _
-- | Match with liability instead of stake.
pattern BetWithLiability b o l <- Bet b o (getLiability -> l)

-- | Given a bet and opposing bet odds, calculates the ideal stake size to
-- minimize potential loss.
--
-- This is useful in bet trading, which gives this function its name.
bestTradingStake :: ( Fractional odds, odds ~ money )
                 => Bet odds money -> odds -> money
bestTradingStake (Bet bt o m) opposing_odds = case bt of
    Back -> o*m / opposing_odds
    Lay -> m*o / opposing_odds

-- | Same as `bestTradingStake` but wraps the result in a new bet.
bestTradingStake2 :: ( Fractional odds, odds ~ money )
                  => Bet odds money -> odds -> Bet odds money
bestTradingStake2 bet opposing_odds =
    Bet (oppositeBetType $ bet^.betType)
        opposing_odds
        (bestTradingStake bet opposing_odds)

instance FromJSON BetType where
    parseJSON (String "BACK") = pure Back
    parseJSON (String "LAY") = pure Lay
    parseJSON _ = empty

instance ToJSON BetType where
    toJSON Back = String "BACK"
    toJSON Lay = String "LAY"


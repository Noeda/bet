-- | Helper functions for template haskell that generate JSON instances for
-- Betfair API types. This is unlikely to be useful to you if you are just a
-- user of this library.
--

module Network.Betfair.Types.TH
    ( commonEnum
    , commonStruct )
    where

import Data.Aeson.TH
import Data.Char

commonStruct :: Int -> Options
commonStruct ln =
    defaultOptions {
        fieldLabelModifier = drop ln
      , omitNothingFields = True
    }

underscorify :: String -> String
underscorify str = reverse $ rec str [] where
  rec [] accum = accum
  rec (x:y:rest) accum
    | isLower x && isUpper y = rec rest $ y:'_':x:accum
  rec (z:rest) accum = rec rest (z:accum)

commonEnum :: Int -> Options
commonEnum ln =
    defaultOptions {
        constructorTagModifier = fmap toUpper . underscorify . drop ln
      , allNullaryToStringTag = True
    }


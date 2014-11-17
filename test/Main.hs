module Main ( main ) where

import qualified DataBet as DB
import Test.Framework

main :: IO ()
main = defaultMain [DB.dataBetTestGroup]


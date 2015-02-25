{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main ( main ) where

import Control.Applicative
import Network.Betfair
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
   args <- getArgs
   if null args
     then usageHelp >> exitFailure
     else performLogin $ head args

performLogin :: FilePath -> IO ()
performLogin fpath = do
    file <- openFile fpath ReadMode
    credentials <- read <$> hGetContents file
    betfair <- openBetfair credentials
    betfair `seq` putStrLn "Successfully logged in!"

usageHelp :: IO ()
usageHelp = do
    putStrLn "Usage: login-test [credentials file]\n"
    putStrLn $ "The credentials file should be a `Read`able " ++
               "`Network.Betfair.Credentials` value. Here's an example:"
    putStrLn $ show $ Credentials
        { _username = "blahblah"
        , _password = "123456789"
        , _apiKey = "1bcj nice api key blarghh"
        , _certificatePrivateKeyFile = "ssl/secret.key"
        , _certificateCertificateFile = "ssl/secret.crt" }



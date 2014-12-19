{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.Loggly
-- Copyright   : (c) Vertigo Media Inc. 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Web.Loggly where

import           Control.Applicative ( (<$>) )
import           Data.Aeson
import           Data.Aeson.Parser
import           Data.List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Network.HTTP.Types
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import           System.IO.Streams.Attoparsec (parseFromStream)
import           System.IO.Streams.HTTP

------------------------------------------------------------------------------
-- | Loggly Token
newtype LogglyToken = LogglyToken String
    deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Loggly Tag
newtype Tag = Tag Text deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Type def for [Tag]
type Tags = [Tag]

------------------------------------------------------------------------------
-- | Loggly configuration settings
data LogglyConfig = LogglyConfig {
        logglyToken   :: LogglyToken
      , logglyManager :: Manager
      } 

------------------------------------------------------------------------------
-- | Show Instance for Loggly
instance Show LogglyConfig where
    show (LogglyConfig token _) = show token

------------------------------------------------------------------------------
-- | Request Object for Loggly
data LogglyRequest = forall a . ToJSON a =>
       LogglyRequest {
         logglyData :: a
       , logglyTags :: [Tag]
       } 

------------------------------------------------------------------------------
-- | Arbitrary Tags to add to loggly request
addTags :: Tags -> String
addTags tags = intercalate "," [ T.unpack x | Tag x <- tags ]

------------------------------------------------------------------------------
-- | Loggly Error
data LogglyError = LogglyError {
        
      } deriving (Show)

------------------------------------------------------------------------------
-- | Send loggly request, HTTPS by default
loggly
  :: LogglyConfig
  -> LogglyRequest
  -> IO ()
loggly LogglyConfig{..} LogglyRequest{..} = do
  let LogglyToken token = logglyToken
  req' <- parseUrl $ mconcat [
                           "https://logs-01.loggly.com/inputs/"
                          , token
                          , "/tag/"
                          , addTags logglyTags
                          ]
  let request = req' { method = "POST"
                     , requestBody = stream $ Streams.fromLazyByteString (encode logglyData)
                     , requestHeaders = [ ("content-type", "application/json") ]
                     } 
  withHTTP request logglyManager $ \response ->
    print =<< parseFromStream json' (responseBody response)

------------------------------------------------------------------------------
-- | Test Person
data Person = Person { name :: String, age :: Int } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

------------------------------------------------------------------------------
-- | test
test :: IO ()
test = flip loggly req =<< getConf
  where
    req = LogglyRequest (Person "dj" 27) [Tag "people"]
    getConf = 
      withManager tlsManagerSettings $ \mgr -> do
        token <- getToken
        return $ LogglyConfig token mgr
    getToken = LogglyToken <$> readFile "/Users/dmj/.loggly"


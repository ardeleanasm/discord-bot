{-# LANGUAGE OverloadedStrings #-}
module ConfigParser (
    parseConfiguration
  , BotConfig(..)
    ) where

import Data.Either
import Data.Aeson
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B



data BotConfig = BotConfig {
    botDiscordToken :: String
} deriving (Show,Eq)


instance FromJSON BotConfig where
    parseJSON (Object v) = 
        BotConfig <$> v.: "botDiscordToken"
    parseJSON _ = mzero

instance ToJSON BotConfig where
    toJSON (BotConfig discordToken) = 
        object [ "botDiscordToken" .= discordToken ]


getJSON :: String -> IO B.ByteString
getJSON file = B.readFile file


parseConfiguration :: String -> IO (Either String BotConfig)
parseConfiguration path = eitherDecode <$> getJSON path


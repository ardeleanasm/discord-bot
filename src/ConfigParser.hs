{-# LANGUAGE OverloadedStrings #-}
module ConfigParser (
    parseConfiguration
  , BotConfig(..)
    ) where

import Data.Either




data BotConfig = BotConfig {
    discordToken :: String,
    lastTime :: String
} deriving (Show,Eq)



parseConfiguration :: String -> BotConfig
parseConfiguration = undefined

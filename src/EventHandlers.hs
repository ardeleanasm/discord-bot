{-# LANGUAGE OverloadedStrings #-}
module EventHandlers (
    onDiscordEventHandler  

    ) where

import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import Control.Concurrent (threadDelay)
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Commands

onDiscordEventHandler :: DiscordHandle -> Event -> IO ()
onDiscordEventHandler dis event = case event of
    MessageCreate m -> executeCommand dis m
    _ -> pure ()



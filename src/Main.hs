{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Main where

import qualified Data.Text.IO as TIO
import Discord
import Discord.Types
import qualified Discord.Requests as R


import EventHandlers

-- | TODO: 
-- 1. Read token from config file
-- 2. Add bark,bribe,bef ( befriend with Eugleniu) commands. Eugleniu will count how many friends has in a file
-- 3. .bef and .bark will work only after some relatively big time intervals, compared to bribe. If bribed, it will respond with eyes
-- 4. uptime feature
-- 5. 


-- | Replies "pong" to every message that starts with "ping"
connect :: IO ()
connect = do 
    userFacingError <- runDiscord $ def 
            { discordToken = "Token"
            , discordOnEvent = onDiscordEventHandler 
            }
    TIO.putStrLn userFacingError


main :: IO ()
main = connect

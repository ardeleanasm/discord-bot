{-# LANGUAGE OverloadedStrings #-}
module Commands (
    executeCommand

    ) where

import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import Control.Concurrent (threadDelay)
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

--executeCommand::DiscordHandle->Message -> IO ()
--executeCommand dis m = when (not (fromBot m) && isPing (messageText m)) $ do
--    _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
--    threadDelay (1 * 10^6)
--    _ <- restCall dis (R.CreateMessage (messageChannel m) "pong!")
--    pure ()


executeCommand :: DiscordHandle -> Message -> IO ()
executeCommand dis m = when (not (fromBot m) ) $ do
    _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
    threadDelay (1 * 10^6)
    _ <- restCall dis (R.CreateMessage (messageChannel m) "pong!")
    pure ()

parseCommand :: Text -> IO ()
parseCommand rxMessage = Nothing


-- | Commands 
uptime = Nothing

bribe = Nothing

befriend = Nothing

bark = Nothing

ping = Nothing


fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower

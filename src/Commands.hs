{-# LANGUAGE OverloadedStrings #-}
module Commands (
    execute
  , AvailableCommand(..)
  , CommandContext(..)
    ) where
import Control.Monad.IO.Class 
import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text,pack)
import Data.Time
import Control.Concurrent (threadDelay)
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R



data AvailableCommand = PING | UPTIME | NONE deriving (Eq, Show)

data CommandContext = CommandContext {
      handler :: DiscordHandle
    , message :: Message
    , startTime :: UTCTime
    } 

execute :: CommandContext -> IO ()
execute cmd = when (not (fromBot $ message cmd)) $ do
    case (parseCommand $ messageText (message cmd)) of
        PING -> pingResponse cmd 
        UPTIME -> uptimeResponse cmd



parseCommand :: Text -> AvailableCommand
parseCommand receivedMessage 
    | isPing receivedMessage    = PING
    | isUptime receivedMessage  = UPTIME
    | otherwise                 = NONE
    


-- | Commands
uptimeResponse :: CommandContext -> IO () 
uptimeResponse cmd= do
    _ <- restCall (handler cmd) (R.CreateReaction (messageChannel (message cmd), messageId (message cmd)) "fire")
    threadDelay (1 * 10^6)
    now <- getCurrentTime
    zero <- return (startTime cmd)
    _ <- restCall (handler cmd) (R.CreateMessage (messageChannel (message cmd)) (pack $ pretty (diffUTCTime now zero)))

    pure ()


pingResponse :: CommandContext -> IO ()
pingResponse cmd = do 
    _ <- restCall (handler cmd) (R.CreateReaction (messageChannel (message cmd), messageId (message cmd)) "eyes")
    threadDelay (1 * 10^6)
    _ <- restCall (handler cmd) (R.CreateMessage (messageChannel (message cmd)) "pong!")
    pure ()


fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower

isUptime :: Text -> Bool
isUptime = ("uptime" `isPrefixOf`) . toLower


pretty :: NominalDiffTime -> String
pretty diff =
    unwords
      . map (\(t, unit) -> show t ++ unit)
      $ if null diffs then [(0, "s")] else diffs
    where
        diffs :: [(Integer, String)]
        diffs = filter ((/= 0) . fst)
            $ decompose [(86400, "d"), (3600, "h"), (60, "m"), (1, "s")] (floor diff)
        decompose [] _ = []
        decompose ((secs, unit) : metrics) t =
            let (n, t') = t `divMod` secs
            in (n, unit) : decompose metrics t' 
